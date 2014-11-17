package com.github.comco.scrappy.checker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

case class AndAlsoChecker(val firstChecker: Checker, val secondChecker: Checker)
    extends BaseChecker {
  require(firstChecker.sourceType == secondChecker.sourceType,
    s"For creation of AndAlsoChecker, the both arguments should have the same source type.")
    
  def sourceType = firstChecker.sourceType
  
  def doCheckData(source: Data.Any): CheckResult = {
    val firstResult = firstChecker.checkData(source)
    if (!firstResult.successful) {
      return firstResult
    } else {
      return secondChecker.checkData(source)
    }
  }
  
  def doCheckOriginatedData(source: OriginatedData.Any): OriginatedCheckResult = {
    val firstResult = firstChecker.checkOriginatedData(source)
    if (!firstResult.successful) {
      return OriginatedCheckResult(false, source.origin, Set(firstResult), this)
    } else {
      val secondResult = secondChecker.checkOriginatedData(source)
      if (secondResult.successful) {        
    	  return OriginatedCheckResult(true, source.origin, Set(firstResult, secondResult), this)
      } else {
        return OriginatedCheckResult(false, source.origin, Set(secondResult), this)
      }
    }
  }
}