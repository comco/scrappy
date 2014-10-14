package com.github.comco.scrappy.checker.quantifier

import com.github.comco.scrappy.checker.quantifier.Quantifier.ContractChecking

class ForAllQuantifier private() extends BaseQuantifier {
  var ok = true
  
  def doPut(result: Boolean): Unit = {
    if (!result) {
      state = Quantifier.Done
      ok = false
    } else {
      state = Quantifier.Current
    }
  }
  
  def isUnusual() = !ok
  
  def isValid() = ok
}

object ForAllQuantifier extends QuantifierFactory {
  def createEmpty() = new ForAllQuantifier()
}