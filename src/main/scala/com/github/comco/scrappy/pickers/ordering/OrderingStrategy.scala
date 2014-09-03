package com.github.comco.scrappy.pickers.ordering
import com.github.comco.scrappy._

abstract class OrderingStrategy {
  def datatype: Type
  def dataOrdering: Ordering[DataDomain.Data]
  def originatedDataOrdering: Ordering[OriginatedDataDomain.Data]
}

object OrderingStrategy {
  trait SpecificDatatypeOrderingStrategy extends OrderingStrategy {
    /**
     * The datatypes should be analogous
     */
    type SpecificData <: DataDomain.Data
    type SpecificOriginatedData <: OriginatedDataDomain.Data

    def doCompareData(x: SpecificData, y: SpecificData): Int
    def doCompareOriginatedData(x: SpecificOriginatedData, y: SpecificOriginatedData): Int

    final lazy val dataOrdering = new Ordering[DataDomain.Data] {
      def compare(x: DataDomain.Data, y: DataDomain.Data) = {
        require(x.datatype == datatype && y.datatype == datatype)
        doCompareData(x.asInstanceOf[SpecificData], y.asInstanceOf[SpecificData])
      }
    }
    
    final lazy val originatedDataOrdering = new Ordering[OriginatedDataDomain.Data] {
      def compare(x: OriginatedDataDomain.Data, y: OriginatedDataDomain.Data) = {
        require(x.datatype == datatype && y.datatype == datatype)
        doCompareOriginatedData(x.asInstanceOf[SpecificOriginatedData], y.asInstanceOf[SpecificOriginatedData])
      }
    }
  }
}