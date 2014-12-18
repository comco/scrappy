package com.github.comco.scrappy

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Represents data checks. Works reasonably both for bare data and for
 * originated data.
 */
abstract class Checker {
  def sourceType: Type

  def checkData(source: Data): CheckResult
  def checkOriginatedData(source: OriginatedData): OriginatedCheckResult
}