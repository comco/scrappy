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
  }
  
  trait BaseTupleData extends BaseData {
    this: TupleData =>
      
    def datatype: TupleType
    def coordinates: IndexedSeq[Data]
  }
  
  trait BaseStructData extends BaseData {
    this: StructData =>
      
    def datatype: StructType
    def features: Map[String, Data]
    
    def hasFeature(name: String) = features.contains(name)
    def feature(name: String) = features(name)
  }
  
  trait BaseSeqData extends BaseData {
    this: SeqData =>
      
    def datatype: SeqType
    def elements: Seq[Data]
    
    def element(index: Int) = elements(index)
    def length: Int = elements.length
  }
}