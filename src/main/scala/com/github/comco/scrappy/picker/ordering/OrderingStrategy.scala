package com.github.comco.scrappy.picker.ordering

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

abstract class OrderingStrategy {
  def datatype: Type
  def dataOrdering: Ordering[Data]
  def originatedDataOrdering: Ordering[OriginatedData]
}

object OrderingStrategy {
  trait SpecificDatatypeOrderingStrategy extends OrderingStrategy {
    /**
     * The datatypes should be analogous
     */
    type SpecificData <: Data
    type SpecificOriginatedData <: OriginatedData

    def doCompareData(x: SpecificData, y: SpecificData): Int
    def doCompareOriginatedData(x: SpecificOriginatedData, y: SpecificOriginatedData): Int

    final lazy val dataOrdering = new Ordering[Data] {
      def compare(x: Data, y: Data) = {
        require(x.datatype == datatype && y.datatype == datatype)
        doCompareData(x.asInstanceOf[SpecificData], y.asInstanceOf[SpecificData])
      }
    }
    
    final lazy val originatedDataOrdering = new Ordering[OriginatedData] {
      def compare(x: OriginatedData, y: OriginatedData) = {
        require(x.datatype == datatype && y.datatype == datatype)
        doCompareOriginatedData(x.asInstanceOf[SpecificOriginatedData], y.asInstanceOf[SpecificOriginatedData])
      }
    }
  }
}