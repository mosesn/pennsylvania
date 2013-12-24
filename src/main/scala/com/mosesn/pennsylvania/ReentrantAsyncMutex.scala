package com.mosesn.pennsylvania

import com.twitter.concurrent.{AsyncSemaphore, Permit}
import com.twitter.util.{Future, Local, Promise}
import com.twitter.util.Local.Context
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap

class ReentrantAsyncMutex private (maxWaiters: Option[Int]) extends AsyncSemaphore(1, maxWaiters) {
  def this() = this(None)
  def this(maxWaiters: Int) = this(Some(maxWaiters))

  private[this] val atomic = new AtomicInteger()
  // is the hashcode sufficient?
  @volatile private[this] var exclusiveOwnerProof: Option[(Context, Permit)] = None
  private[this] val waiters = TrieMap[Context, List[Promise[Permit]]]()

  override def acquire(): Future[Permit] = {
    val curCtx = Local.save()
    // TODO: this can be more granular
    synchronized {
      exclusiveOwnerProof match {
        case Some((ctx, permit)) if ctx == curCtx => {
          atomic.incrementAndGet()
          Future.value(new ReentrantPermit(permit))
        }
        case _ => {
          waiters.get(curCtx) match {
            case None => {
              waiters += (curCtx -> Nil)
              val f = super.acquire() map { permit =>
                // TODO: granularity
                synchronized {
                  waiters(curCtx) foreach { promise =>
                    promise.setValue(new ReentrantPermit(permit))
                    atomic.incrementAndGet()
                  }
                  waiters -= curCtx
                  exclusiveOwnerProof = Some(curCtx -> permit)
                  atomic.incrementAndGet()
                  new ReentrantPermit(permit)
                }
              }
              f
            }
            case Some(list) => {
              val p = Promise[Permit]()
              waiters += curCtx -> (p :: list)
              p
            }
          }
        }
      }
    }
  }

  class ReentrantPermit(permit: Permit) extends Permit {
    def release() {
      // TODO: granularity
      synchronized {
        if (atomic.decrementAndGet() == 0) {
          exclusiveOwnerProof = None
          permit.release()
        }
      }
    }
  }
}
