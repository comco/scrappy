package com.github.comco.scrappy.checker.quantifier

import com.github.comco.scrappy.checker.Checker
import com.github.comco.scrappy.checker.BaseSeqChecker
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.checker.CheckResult
import com.github.comco.scrappy.checker.CheckResult
import com.github.comco.scrappy.checker.OriginatedCheckResult
import com.github.comco.scrappy.originated_data.OriginatedSeqData

/**
 * Not thread-safe!
 */
case class QuantifierChecker(val quantifierFactory: QuantifierFactory, val elementChecker: Checker) 
  extends BaseSeqChecker with Marking {
  def sourceType = SeqType(elementChecker.sourceType)
  
  def doCheckData(source: SeqData): CheckResult = {
    val quantifier = quantifierFactory.create()
    for (element <- source.elements) {
      val result = elementChecker.checkData(element)
      quantifier.addResult(result.successful)
      if (quantifier.state == Quantifier.Done) {
        return CheckResult(quantifier.successful)
      }
    }
    quantifier.finish()
    return CheckResult(quantifier.successful)
  }
  
  def doCheckOriginatedData(source: OriginatedSeqData): OriginatedCheckResult = {
    ??? // TODO
    // Need ThreadLocal store for the origins
  }
  
  def markCurrentResult(): Unit = ???
}