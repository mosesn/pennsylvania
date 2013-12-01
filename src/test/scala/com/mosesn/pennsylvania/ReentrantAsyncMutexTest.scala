package com.mosesn.pennsylvania

import com.twitter.util.Await
import org.scalatest.{FunSpec, ShouldMatchers}

class ReentrantAsyncMutexTest extends FunSpec with ShouldMatchers {
  describe("ReentrantAsyncMutex") {
    it("should support reentrancy") {
      val mutex = new ReentrantAsyncMutex()
      val permit = mutex.acquire()
//      val permit = Await.result(mutex.acquire())
      permit should be ('defined)
      val f = mutex.acquire()
      f should be ('defined)
    }
  }
}
