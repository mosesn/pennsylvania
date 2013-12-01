package com.mosesn.pennsylvania

import com.twitter.concurrent.{AsyncSemaphore, Permit}
import com.twitter.util.{Future, Local, Promise}
import com.twitter.util.Local.Context
import scala.collection.concurrent.TrieMap

class ReentrantAsyncMutex private (maxWaiters: Option[Int]) extends AsyncSemaphore(1, maxWaiters) {
  def this() = this(None)
  def this(maxWaiters: Int) = this(Some(maxWaiters))

  // is the hashcode sufficient?
  @volatile private[this] var exclusiveOwnerProof: Option[(Context, Permit)] = None
  private[this] val waiters = TrieMap[Context, List[Promise[Permit]]]()

  override def acquire(): Future[Permit] = {
    val curCtx = Local.save()
    exclusiveOwnerProof match {
      case Some((ctx, permit)) if ctx == curCtx => Future.value(permit)
      case _ => {
        waiters.get(curCtx) match {
          case None => {
            waiters += (curCtx -> Nil)
            val f = super.acquire() map { permit =>
              waiters(curCtx) foreach { promise => promise.setValue(permit) }
              waiters -= curCtx
              exclusiveOwnerProof = Some(curCtx -> permit)
              new ReentrantPermit(permit)
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

  class ReentrantPermit(permit: Permit) extends Permit {
    // TODO: this should have a counter
    def release() {
      exclusiveOwnerProof = None
      permit.release()
    }
  }
}
