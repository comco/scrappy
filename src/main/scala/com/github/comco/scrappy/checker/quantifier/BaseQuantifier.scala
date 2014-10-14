package com.github.comco.scrappy.checker.quantifier

import Quantifier._

abstract class BaseQuantifier extends Quantifier {
  var state: Quantifier.State = Quantifier.Empty

  def put(next: Boolean): Unit = {
    require(state != Done,
      "The state of a Quantifier should not be Done for putting.")
    doPut(next) ensuring (state != Empty, 
        "After putting, the Quantifier should not be Empty.")
  }

  def doPut(next: Boolean): Unit

  def unusual: Boolean = {
    require(state != Empty,
      "Only Quantifiers in non-Emply state can be queried by unusual.")
    val oldState = state
    isUnusual() ensuring (state == oldState,
      "After querying for unusual, the state should stay the same.")
  }

  def isUnusual(): Boolean

  def finish(): Unit = {
    require(state != Done, "Quantifier state is already Done.")
    doFinish()
    state = Quantifier.Done
  }
  
  def doFinish(): Unit = {}

  def valid: Boolean = {
    require(state == Done, "Quantifier state is already Done.")
    isValid() ensuring(state == Done,
        "After valid, the state should stay Done.")
  }
  
  def isValid(): Boolean
}