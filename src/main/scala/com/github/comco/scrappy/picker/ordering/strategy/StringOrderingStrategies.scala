package com.github.comco.scrappy.picker.ordering.strategy

import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.picker.ordering.OrderingStrategy

object StringOrderingStrategies extends BaseOrderingStrategies {
  case object Ascending
      extends OrderingStrategy
      with OrderingStrategy.SpecificDatatypeOrderingStrategy {
    type SpecificData = PrimitiveData[String]
    type SpecificOriginatedData = OriginatedPrimitiveData[String]

    def datatype = StringPrimitiveType

    def doCompareData(x: PrimitiveData[String],
      y: PrimitiveData[String]): Int = {
      x.value compare (y.value)
    }

    def doCompareOriginatedData(x: OriginatedPrimitiveData[String],
      y: OriginatedPrimitiveData[String]): Int = {
      x.value compare (y.value)
    }
  }
}