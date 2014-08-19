package com.github.comco.scrappy

import scala.language.higherKinds

trait Domain {
  type Data <: BaseData
  type PrimitiveData[T] <: BasePrimitiveData[T]
  type TupleData <: BaseTupleData
  type StructData <: BaseStructData
  type SeqData <: BaseSeqData
  
  abstract class BaseData {
    def datatype: Type
  }
  
  trait BasePrimitiveData[T] extends BaseData {
    this: PrimitiveData[T] =>
      
    def datatype: PrimitiveType[T]
    def value: T
  }
  
  trait BaseTupleData extends BaseData {
    this: TupleData =>
      
    def datatype: TupleType
    def coordinates: IndexedSeq[Data]
    
    def size: Int = datatype.size
    
    def hasCoordinate(position: Int): Boolean = {
      datatype.hasCoordinate(position) && coordinates(position) != null
    }
    
    def coordinate(position: Int): Data = {
      require(datatype.hasCoordinate(position), s"Coordinate position: $position is out of bounds for TupleType: $datatype")
      require(coordinates(position) != null, s"TupleData doesn't contain a coordinate at position: $position")
      coordinates(position)
    }
  }
  
  trait BaseStructData extends BaseData {
    this: StructData =>
      
    def datatype: StructType
    def features: Map[String, Data]
    
    def hasFeature(name: String): Boolean = {
      features.contains(name) && features(name) != null
    }
    def feature(name: String): Data = {
      require(hasFeature(name), s"StructData doesn't contain a feature named: $name")
      features(name)
    }
  }
  
  trait BaseSeqData extends BaseData {
    this: SeqData =>
      
    def datatype: SeqType
    def elements: Seq[Data]
    
    def hasElement(index: Int): Boolean = {
      0 <= index && index < length && elements(index) != null
    }
    
    def element(index: Int): Data = {
      require(0 <= index && index < length, s"Index: $index is out of bounds for SeqData with length: $length.")
      require(elements(index) != null, s"SeqData doesn't contain an element at index: $index")
      elements(index)
    }
    
    def length: Int = elements.length
  }
}