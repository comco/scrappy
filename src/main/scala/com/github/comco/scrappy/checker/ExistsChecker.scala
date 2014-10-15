package com.github.comco.scrappy.checker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

case class ExistsChecker(val elementChecker: Checker) extends BaseSeqChecker {
  def sourceType = SeqType(elementChecker.sourceType)

  def doCheckData(data: SeqData): CheckResult = {
    val result = data.elements.exists(elementChecker.checkData(_).successful)
    return CheckResult(result)
  }
  
  def doCheckOriginatedData(data: OriginatedSeqData): OriginatedCheckResult = {
    for (element <- data.elements) {
      val result = elementChecker.checkOriginatedData(element)
      if (result.successful) {
        return OriginatedCheckResult(true, data.origin, Set(result), this)
      }
    }
    return OriginatedCheckResult(false, data.origin, Set.empty, this)
  }
}