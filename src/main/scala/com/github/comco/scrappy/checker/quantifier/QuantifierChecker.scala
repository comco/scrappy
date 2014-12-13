package com.github.comco.scrappy.checker.quantifier

import com.github.comco.scrappy.checker.Checker
import com.github.comco.scrappy.checker.BaseSeqChecker
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.checker.CheckResult
import com.github.comco.scrappy.checker.CheckResult
import com.github.comco.scrappy.checker.OriginatedCheckResult
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.checker.OriginatedCheckResult
import com.github.comco.scrappy.origin.ComputedOrigin
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.checker.CheckReason

case class QuantifierChecker(val quantifierFactory: QuantifierFactory, val elementChecker: Checker) 
  extends BaseSeqChecker {
  def sourceType = SeqType(elementChecker.sourceType)
  
  def doCheckData(source: SeqData): CheckResult = {
    val quantifier = quantifierFactory.createEmpty()
    for (element <- source.elements) {
      val result = elementChecker.checkData(element)
      quantifier.put(result.successful)
      if (quantifier.state == Quantifier.Done) {
        return CheckResult(quantifier.valid)
      }
    }
    quantifier.finish()
    return CheckResult(quantifier.valid)
  }
  
  def doCheckOriginatedData(source: OriginatedSeqData): OriginatedCheckResult = {
    val quantifier = quantifierFactory.createEmpty()
    var witnesses = Set.empty[CheckReason]
    for (element <- source.elements) {
      val result = elementChecker.checkOriginatedData(element)
      quantifier.put(result.successful)
      if (quantifier.unusual) {
        witnesses += result
      }
      if (quantifier.state == Quantifier.Done) {
        return OriginatedCheckResult(quantifier.valid, source.origin, witnesses, this)
      }
    }
    quantifier.finish()
    return OriginatedCheckResult(quantifier.valid, source.origin, witnesses, this)
  }
}