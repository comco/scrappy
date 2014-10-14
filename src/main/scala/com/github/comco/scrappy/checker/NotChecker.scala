package com.github.comco.scrappy.checker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

case class NotChecker(val checker: Checker) extends BaseChecker {
  def sourceType = checker.sourceType
  
  def doCheckData(source: Data): CheckResult = {
    val result = checker.checkData(source)
    return result.copy(successful = !result.successful)
  }
  
  def doCheckOriginatedData(source: OriginatedData): OriginatedCheckResult = {
    val result = checker.checkOriginatedData(source)
    return OriginatedCheckResult(!result.successful, source.origin, Set(result), this)
  }
}