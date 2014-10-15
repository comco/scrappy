package com.github.comco.scrappy.checker.quantifier

/**
 * A Quantifier is an abstration of a traversion policy of a sequence with regards
 * to a condition. For instance, for every, none, some, exactly one can be modeled
 * as Quantifiers. A Quantifier can provide early breaking (when in state Done),
 * and identification of unusual points.
 * A Quantifier should be driven as a continuation with the following pattern:
 * first obtain it from a factory, then call put / unusual until the state is
 * finished.
 * 
 * MUTABLE! (Don't expect thread-safety).
 */
abstract class Quantifier {
  def state: Quantifier.State
  
  def put(next: Boolean): Unit
  
  def unusual: Boolean
  
  def finish(): Unit
  
  def valid: Boolean
}

abstract class QuantifierFactory {
  def createEmpty(): Quantifier
}

object Quantifier extends Enumeration {
  type State = Value
  
  val Empty = Value
  val Current = Value
  val Done = Value
}