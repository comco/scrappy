package com.github.comco.scrappy.checker

import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.PrimitiveType

case class PredicateChecker[T](pred: T => Boolean)(implicit val sourceType: PrimitiveType[T]) extends BasePrimitiveChecker[T] {
  def doCheckData(source: PrimitiveData[T]): CheckResult = {
    CheckResult(pred(source.value))
  }
  
  def doCheckOriginatedData(source: OriginatedPrimitiveData[T]): OriginatedCheckResult = {
    val successful = pred(source.value)
    val scope = source.origin
    val witnesses = Set.empty[CheckReason]
    val checker = this
    OriginatedCheckResult(successful, scope, witnesses, checker)
  }
}