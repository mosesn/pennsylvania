package com.mosesn.pennsylvania

trait Rule[A] {
  def transitions: Map[(A, A), Seq[A => Unit]]
}

object Rule {
  def consolidate[A](rules: Seq[Rule[A]]): Rule[A] = new Rule[A] {
    def transitions: Map[(A, A), Seq[A => Unit]] = {
      val map: Map[(A, A), Seq[((A, A), Seq[A => Unit])]] = rules.map { _.transitions.toSeq }.reduce { _ ++ _ }.groupBy[(A, A)] { case (key, value) => key }
      map.map { case (key, value) => key -> (value.map { _._2 } reduce (_ ++ _)) }
    }
  }
}

class StartsAt[A](start: A) extends Rule[A] {
  def transitions: Map[(A, A), Seq[A => Unit]] = Map[(A, A), Seq[A => Unit]]((null.asInstanceOf[A], start) -> Seq.empty[A => Unit])
}

class GoesTo[A](from: A, to: Set[A]) extends Rule[A] {
  def this(from: A, to: A) = this(from, Set(to))

  def transitions: Map[(A, A), Seq[A => Unit]] = (for {
    t <- to
  } yield (from, t) -> Seq.empty[A => Unit]).toMap
}

class Transition[A](from: A, to: Set[(A, A => Unit)]) extends Rule[A] {
  def this(from: A, to: Set[A], fn: A => Unit) = this(from, to map { _ -> fn })

  def transitions: Map[(A, A), Seq[A => Unit]] = (for {
    (t, fn) <- to
  } yield (from, t) -> Seq(fn)).toMap
}

class Rulebook[A](rule: Rule[A]) {
  def this(rules: Seq[Rule[A]]) = this(Rule.consolidate(rules))

  def permits(from: A, to: A): Boolean = (from == to) || rule.transitions.keys.toSet.contains((from, to))

  def transition(from: A, to: A) {
    rule.transitions((from, to)).foreach { _(to) }
  }

  // TODO: friendlier error
  def start: A = rule.transitions.keys.find(_._1 == null).get._2
}
