package com.mosesn.pennsylvania

import com.twitter.util.{Await, Local}
import com.twitter.util.Local.Context
import org.scalatest.{FunSpec, ShouldMatchers}

class ReentrantAsyncMutexTest extends FunSpec with ShouldMatchers {
  describe("ReentrantAsyncMutex") {
    // TODO: validate it does the right thing on late acquisition with multiple waiters

    it("should support reentrancy") {
      val mutex = new ReentrantAsyncMutex()
      val permit = mutex.acquire()
      permit should be ('defined)
      val f = mutex.acquire()
      f should be ('defined)
    }

    it("should be reentrant on reacquisition") {
      val mutex = new ReentrantAsyncMutex()
      val permit = mutex.acquire()
      permit should be ('defined)
      Local.restore(new Context(0))
      val f1 = mutex.acquire()
      f1 should not be ('defined)
      Await.result(permit).release()
      f1 should be ('defined)
      val f2 = mutex.acquire()
      f2 should be ('defined)
    }

    it("should count properly") {
      val mutex = new ReentrantAsyncMutex()
      val permit = mutex.acquire()
      permit should be ('defined)
      val f = mutex.acquire()
      f should be ('defined)
      Await.result(permit).release()

      val saved = Local.save()

      Local.restore(new Context(0))
      val waiter = mutex.acquire()
      waiter should not be ('defined)

      Await.result(f).release()

      waiter should be ('defined)
    }

    it("should not care about order") {
      val mutex = new ReentrantAsyncMutex()
      val permit = mutex.acquire()
      permit should be ('defined)
      val f = mutex.acquire()
      f should be ('defined)
      Await.result(f).release()

      val saved = Local.save()

      Local.restore(new Context(0))
      val waiter = mutex.acquire()
      waiter should not be ('defined)

      Await.result(permit).release()

      waiter should be ('defined)
    }
  }
}
