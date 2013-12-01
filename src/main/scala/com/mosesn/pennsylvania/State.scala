package com.mosesn.pennsylvania

import com.twitter.util.{Extractable, Future, Updatable, Var}
import com.twitter.concurrent.AsyncMutex

trait State[A] {
  // TODO: there must be a name in the literature for this
  def send(next: A): Future[Boolean]

  def state: Var[A] with Extractable[A]
}

object State {
  def mk[A](rules: Rule[A]): State[A] = mk(new Rulebook(rules))
  def mk[A](rules: Seq[Rule[A]]): State[A] = mk(new Rulebook(rules))
  def mk[A](rulebook: Rulebook[A]): State[A] = new RuledState(rulebook)
}

class RuledState[A](rules: Rulebook[A]) extends State[A] {
  private[this] val mutex = new ReentrantAsyncMutex()

  private[this] val underlying: Var[A] with Updatable[A] with Extractable[A] = Var(rules.start)

  def send(next: A): Future[Boolean] = mutex.acquire() map { permit =>
    val canTransition = rules.permits(state(), next)
    val old = underlying()
    if (canTransition && old != next) {
      underlying() = next
      rules.transition(old, next)
    }
    permit.release()
    canTransition
  }

  def state: Var[A] with Extractable[A] = underlying
}
