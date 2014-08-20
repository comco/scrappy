package com.github.comco.scrappy

import scala.language.higherKinds

/**
 * Representation of a polymorphic hierarchy of data with a type, supporting
 * object algebraic polymorphism.
 */
trait Domain {

  /**
   * Specific base class for data in the domain.
   */
  type Data <: BaseData

  /**
   * Specific class for data of primitive type.
   */
  type PrimitiveData[T] <: BasePrimitiveData[T]

  /**
   * Specific class for data of tuple type.
   */
  type TupleData <: BaseTupleData

  /**
   * Specific class for data of struct type.
   */
  type StructData <: BaseStructData

  /**
   * Specific class for data of seq type.
   */
  type SeqData <: BaseSeqData

  /**
   * Specific class for optional data.
   */
  type OptionData <: BaseOptionData

  type SomeData <: OptionData

  type NoneData <: OptionData

  /**
   * Base class with general properties of all data, independent of domain.
   */
  abstract class BaseData {
    def datatype: Type
  }

  /**
   * Base mixin for primitive data.
   */
  trait BasePrimitiveData[T] extends BaseData {
    this: PrimitiveData[T] =>

    def datatype: PrimitiveType[T]
    def value: T
  }

  /**
   * Base mixin for tuple data.
   */
  trait BaseTupleData extends BaseData {
    this: TupleData =>

    def datatype: TupleType
    def coordinates: IndexedSeq[Data]

    def size: Int = datatype.size

    def hasCoordinate(position: Int): Boolean = {
      datatype.hasCoordinate(position) && isFilled(coordinates(position))
    }

    def coordinate(position: Int): Data = {
      require(datatype.hasCoordinate(position),
        s"Coordinate position: $position is out of bounds for TupleType: $datatype")

      coordinates(position)
    }
  }

  /**
   * Base mixin for struct data.
   */
  trait BaseStructData extends BaseData {
    this: StructData =>

    def datatype: StructType
    def features: Map[String, Data]

    def hasFeature(name: String): Boolean = {
      features.contains(name) && isFilled(features(name))
    }

    def feature(name: String): Data = {
      require(datatype.hasFeature(name), s"StructData doesn't contain a feature named: $name")

      features(name)
    }
  }

  /**
   * Base mixin for seq data.
   */
  trait BaseSeqData extends BaseData {
    this: SeqData =>

    def datatype: SeqType
    def elements: Seq[Data]

    def hasElement(index: Int): Boolean = {
      0 <= index && index < length && isFilled(elements(index))
    }

    def element(index: Int): Data = {
      require(0 <= index && index < length,
        s"Index: $index is out of bounds for SeqData with length: $length.")

      elements(index)
    }

    def length: Int = elements.length
  }

  trait BaseOptionData extends BaseData {
    this: OptionData =>
    
    def datatype: OptionType
    def isSome: Boolean
  }

  trait BaseNoneData extends BaseOptionData {
    this: NoneData =>
    
      def isSome = false
  }

  trait BaseSomeData extends BaseOptionData {
    this: SomeData =>
    
    def isSome = true
    def value: Data
  }
  
  def isFilled(data: Data): Boolean = data match {
    case data: BaseOptionData => data.isSome
    case _ => true
  }
}