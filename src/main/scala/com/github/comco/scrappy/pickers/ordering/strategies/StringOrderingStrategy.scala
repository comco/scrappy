package com.github.comco.scrappy.pickers.ordering.strategies

import com.github.comco.scrappy._
import com.github.comco.scrappy.pickers.ordering.OrderingStrategy
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType

object StringOrderingStrategy {
  case object Ascending
      extends OrderingStrategy
      with OrderingStrategy.SpecificDatatypeOrderingStrategy {
    type SpecificData = DataDomain.PrimitiveData[String]
    type SpecificOriginatedData = OriginatedDataDomain.PrimitiveData[String]
    
    def datatype = StringPrimitiveType

    def doCompareData(x: DataDomain.PrimitiveData[String],
      y: DataDomain.PrimitiveData[String]): Int = {
      x.value.compare(y.value)
    }

    def doCompareOriginatedData(x: OriginatedDataDomain.PrimitiveData[String],
      y: OriginatedDataDomain.PrimitiveData[String]): Int = {
      x.value.compare(y.value)
    }
  }

  final val Descending = DescendingOrderingStrategy(Ascending)
}