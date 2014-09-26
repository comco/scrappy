package com.github.comco.scrappy.picker.ordering.strategy

import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.picker.ordering.OrderingStrategy

object IntOrderingStrategies extends BaseOrderingStrategies {
  case object Ascending
      extends OrderingStrategy
      with OrderingStrategy.SpecificDatatypeOrderingStrategy {
    type SpecificData = PrimitiveData[Int]
    type SpecificOriginatedData = OriginatedPrimitiveData[Int]
    
    def datatype = IntPrimitiveType

    def doCompareData(x: PrimitiveData[Int],
      y: PrimitiveData[Int]): Int = {
      x.value.compare(y.value)
    }

    def doCompareOriginatedData(x: OriginatedPrimitiveData[Int],
      y: OriginatedPrimitiveData[Int]): Int = {
      x.value.compare(y.value)
    }
  }
}