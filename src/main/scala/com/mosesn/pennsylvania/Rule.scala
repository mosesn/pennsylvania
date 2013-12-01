package com.mosesn.pennsylvania

trait Rule[A] {
  def paths: Set[(A, A)]
}

object Rule {
  def consolidate[A](rules: Seq[Rule[A]]): Rule[A] = new Rule[A] {
    val paths: Set[(A, A)] = rules.map { _.paths }.reduce { _ union _ }
  }
}

class GoesTo[A](from: Set[A], to: Set[A]) extends Rule[A] {
  def this(from: Set[A], to: A) = this(from, Set(to))
  def this(from: A, to: Set[A]) = this(Set(from), to)
  def this(from: A, to: A) = this(Set(from), Set(to))

  def paths: Set[(A, A)] = for {
    f <- from
    t <- to
  } yield (f, t)
}

class Rulebook[A](rule: Rule[A]) {
  def permits(from: A, to: A): Boolean = (from == to) || rule.paths.contains((from, to))
  def start: A = rule.paths.find(_._1 == null).get._2
}
