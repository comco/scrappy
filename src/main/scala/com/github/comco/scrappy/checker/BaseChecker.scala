package com.github.comco.scrappy.checker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Base class for general checkers.
 */
abstract class BaseChecker extends Checker {
  def checkData(source: Data): CheckResult = {
    require(source.datatype == sourceType,
        s"Checker $this cannot be applied to source: $source")
    doCheckData(source)
  }
  
  def doCheckData(source: Data): CheckResult
  
  def checkOriginatedData(source: OriginatedData): OriginatedCheckResult = {
    require(source.datatype == sourceType,
        s"Checker $this cannot be applied to source: $source")
    doCheckOriginatedData(source)
  }
  
  def doCheckOriginatedData(source: OriginatedData): OriginatedCheckResult
}