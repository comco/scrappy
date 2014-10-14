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
  
  trait ContractChecking extends Quantifier {
    abstract override def put(next: Boolean): Unit = {
      require(state != Done,
          "The state of a Quantifier should not be Done for putting.")
      super.put(next)
    } ensuring(state != Empty, "After putting, the Quantifier should not be Empty.")
    
    abstract override def unusual: Boolean = {
      require(state != Empty,
          "Only Quantifiers in non-Emply state can be queried by unusual.")
      super.unusual
    } ensuring(state == Current,
        "After queriying for unusual, the state must not change.")
    
    abstract override def finish(): Unit = {
      require(state != Done, "Quantifier state is already Done.")
      super.finish()
    } ensuring(state == Done,
        "After finishing, the state of a Quantifier should be Done.")
        
    abstract override def valid: Boolean = {
      require(state == Done, "Quantifier state should be Done.")
      super.valid
    } ensuring(state == Done, "After valid, the state should stay Done.")
  }
}