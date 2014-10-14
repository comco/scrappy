package com.github.comco.scrappy.checker.quantifier

import com.github.comco.scrappy.checker.quantifier.Quantifier.ContractChecking

class ForSomeQuantifier private() extends Quantifier {
  var state = Quantifier.Empty
  var ok = false
  
  def put(result: Boolean) {
    if (result) {
      state = Quantifier.Done
      ok = true
    } else {
      state = Quantifier.Current
    }
  }
  
  def unusual = ok
  
  def finish() {
    state = Quantifier.Done
  }
  
  def valid = ok
}

object ForSomeQuantifier extends QuantifierFactory {
  def createEmpty() = new ForSomeQuantifier() with ContractChecking
}