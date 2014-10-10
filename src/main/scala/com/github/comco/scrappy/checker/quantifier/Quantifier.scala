package com.github.comco.scrappy.checker.quantifier

abstract class Quantifier {
  val marking: Marking
  
  def state: Quantifier.State
  
  def addResult(result: Boolean): Unit
  
  def finish(): Unit
  
  def successful: Boolean
}

abstract class QuantifierFactory {
  def create(marking: Marking = Marking.Empty): Quantifier
}

object Quantifier extends Enumeration {
  type State = Value
  val Running = Value
  val Done = Value
  
  trait ContractChecking extends Quantifier {
    abstract override def addResult(result: Boolean): Unit = {
      require(state == Running, "Cannot addResult to a non-running Quantifier.")
      super.addResult(result)
    }
    
    abstract override def finish(): Unit = {
      require(state == Running, "Cannot finish a non-running Quantifier.")
      super.finish()
    } ensuring(state == Done, "After finishing, a Quantifier should be in state Done.")
    
    abstract override def successful: Boolean = {
      require(state == Done, "Cannot get result of a non-finished Quantifier.")
      super.successful
    }
  }
}