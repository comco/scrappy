package com.github.comco.scrappy.checker

import com.github.comco.scrappy.picker.Picker
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * First picks data, then checks it.
 */
case class PickingChecker(val picker: Picker, val checker: Checker)
    extends BaseChecker {
  require(picker.targetType == checker.sourceType)
  
  def sourceType = picker.sourceType
  
  def doCheckData(source: Data): CheckResult = {
    val target = picker.pickData(source)
    return checker.checkData(target)
  }
  
  def doCheckOriginatedData(source: OriginatedData): OriginatedCheckResult = {
    val target = picker.pickOriginatedData(source)
    val result = checker.checkOriginatedData(target)
    return OriginatedCheckResult(result.successful, source.origin, Set(result), this)
  }
}