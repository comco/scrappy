package com.github.comco.scrappy.checker.quantifier

import com.github.comco.scrappy.checker.quantifier.Quantifier.ContractChecking

class ForSomeQuantifier private() extends BaseQuantifier {
  var ok = false
  
  def doPut(result: Boolean) {
    if (result) {
      state = Quantifier.Done
      ok = true
    } else {
      state = Quantifier.Current
    }
  }
  
  def isUnusual() = ok
  
  def isValid() = ok
}

object ForSomeQuantifier extends QuantifierFactory {
  def createEmpty() = new ForSomeQuantifier()
}