package com.github.comco.scrappy.checker.quantifier

import com.github.comco.scrappy.checker.quantifier.Quantifier.ContractChecking

class ForAllQuantifier private() extends Quantifier {
  var state = Quantifier.Empty
  var ok = true
  
  def put(result: Boolean): Unit = {
    if (!result) {
      state = Quantifier.Done
      ok = false
    } else {
      state = Quantifier.Current
    }
  }
  
  def unusual = !ok
  
  def finish() {
    state = Quantifier.Done
  }
  
  def valid = ok
}

object ForAllQuantifier extends QuantifierFactory {
  def createEmpty() = new ForAllQuantifier() with ContractChecking
}