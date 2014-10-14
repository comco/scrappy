package com.github.comco.scrappy.checker.quantifier

class ForExactlyQuantifier private(val count: Int) extends Quantifier {
  require(count >= 0, s"Positive count is expected instead of: $count")
  
  var state = Quantifier.Empty
  var hits = 0
  var currentResult = false
  
  def put(result: Boolean) {
    currentResult = result
    if (result) hits += 1
    if (hits > count) {
      state = Quantifier.Done
    } else {
      state = Quantifier.Current
    }
  }
  
  def unusual = currentResult
  
  def finish() {
    state = Quantifier.Done
  }
  
  def valid = (hits == count)
  
}

object ForExactlyQuantifier {
  def from(count: Int) = new QuantifierFactory {
    def createEmpty() = new ForExactlyQuantifier(count) with Quantifier.ContractChecking
  }
}