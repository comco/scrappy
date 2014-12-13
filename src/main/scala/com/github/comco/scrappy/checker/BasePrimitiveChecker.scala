package com.github.comco.scrappy.checker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData

abstract class BasePrimitiveChecker[T] extends BaseChecker {
  def doCheckData(source: Data): CheckResult = {
    doCheckData(source.asInstanceOf[PrimitiveData[T]])
  }
  
  def doCheckData(source: PrimitiveData[T]): CheckResult
  
  def doCheckOriginatedData(source: OriginatedData): OriginatedCheckResult = {
    doCheckOriginatedData(source.asInstanceOf[OriginatedPrimitiveData[T]])
  }
  
  def doCheckOriginatedData(source: OriginatedPrimitiveData[T]): OriginatedCheckResult
}