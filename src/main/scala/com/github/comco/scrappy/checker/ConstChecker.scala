package com.github.comco.scrappy.checker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

case class ConstChecker(val sourceType: Type, val sucessful: Boolean) extends BaseChecker {
  def doCheckData(source: Data) = CheckResult(sucessful)
  
  def doCheckOriginatedData(source: OriginatedData) = 
    OriginatedCheckResult(sucessful, source.origin, Set.empty, this)
}