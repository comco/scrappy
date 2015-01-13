package com.github.comco.scrappy.utils.quantifier

class ForExactlyQuantifier private (val count: Int) extends BaseQuantifier {
  var hits = 0
  var currentResult = false

  def doPut(result: Boolean) {
    currentResult = result
    if (result) hits += 1
    if (hits > count) {
      state = Quantifier.Done
    } else {
      state = Quantifier.Current
    }
  }

  def isUnusual() = currentResult

  def isValid() = (hits == count)

}

object ForExactlyQuantifier {
  def from(count: Int) = new QuantifierFactory {
    require(count >= 0, s"Positive count is expected instead of: $count")
    def createEmpty() = new ForExactlyQuantifier(count)
  }
}