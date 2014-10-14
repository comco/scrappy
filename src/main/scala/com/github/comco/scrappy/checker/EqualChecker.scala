package com.github.comco.scrappy.checker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.picker.Picker

case class EqualChecker(val firstPicker: Picker, val secondPicker: Picker) extends BaseChecker {
  require(firstPicker.sourceType == secondPicker.sourceType && firstPicker.targetType == secondPicker.targetType,
      s"The two pickers: $firstPicker and $secondPicker should be compatible.")
      
  def sourceType = firstPicker.sourceType
  
  def doCheckData(source: Data): CheckResult = {
    val firstResult = firstPicker.pickData(source)
    val secondResult = secondPicker.pickData(source)
    val equal = (firstResult == secondResult)
    return CheckResult(equal)
  }
  
  def doCheckOriginatedData(source: OriginatedData): OriginatedCheckResult = {
    val firstResult = firstPicker.pickOriginatedData(source)
    val secondResult = secondPicker.pickOriginatedData(source)
    val equal = (firstResult.data == secondResult.data)
    val firstWitness = WitnessReason(firstResult.origin)
    val secondWitness = WitnessReason(secondResult.origin)
    return OriginatedCheckResult(equal, source.origin, Set(firstWitness, secondWitness), this)
  }
}