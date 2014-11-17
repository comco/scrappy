package com.github.comco.scrappy.checker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Represents data checks. Works reasonably both for bare data and for
 * originated data.
 */
abstract class Checker {
  def sourceType: Type.Any
  
  def checkData(source: Data.Any): CheckResult
  def checkOriginatedData(source: OriginatedData.Any): OriginatedCheckResult
}