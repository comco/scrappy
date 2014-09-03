package com.github.comco.scrappy.pickers.ordering.strategies

import com.github.comco.scrappy._
import com.github.comco.scrappy.pickers.ordering.OrderingStrategy
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType

object IntOrderingStrategies extends BaseOrderingStrategies {
  case object Ascending
      extends OrderingStrategy
      with OrderingStrategy.SpecificDatatypeOrderingStrategy {
    type SpecificData = DataDomain.PrimitiveData[Int]
    type SpecificOriginatedData = OriginatedDataDomain.PrimitiveData[Int]
    
    def datatype = IntPrimitiveType

    def doCompareData(x: DataDomain.PrimitiveData[Int],
      y: DataDomain.PrimitiveData[Int]): Int = {
      x.value.compare(y.value)
    }

    def doCompareOriginatedData(x: OriginatedDataDomain.PrimitiveData[Int],
      y: OriginatedDataDomain.PrimitiveData[Int]): Int = {
      x.value.compare(y.value)
    }
  }
}