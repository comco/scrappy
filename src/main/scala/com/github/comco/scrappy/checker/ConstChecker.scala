package com.github.comco.scrappy.checker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

case class ConstChecker(val sourceType: Type.Any, val sucessful: Boolean) extends BaseChecker {
  def doCheckData(source: Data.Any) = CheckResult(sucessful)
  
  def doCheckOriginatedData(source: OriginatedData.Any) = 
    OriginatedCheckResult(sucessful, source.origin, Set.empty, this)
}