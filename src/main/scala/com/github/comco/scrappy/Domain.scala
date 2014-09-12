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
  type PrimitiveData[T] <: PrimitiveDataMixin[T]

  /**
   * Specific class for data of tuple type.
   */
  type TupleData <: TupleDataMixin

  /**
   * Specific class for data of struct type.
   */
  type StructData <: StructDataMixin

  /**
   * Specific class for data of seq type.
   */
  type SeqData <: SeqDataMixin

  /**
   * Specific class for optional data.
   */
  type OptionData <: OptionDataMixin
  
  /**
   * Specific class for the case of some optional data.
   */
  type SomeData <: OptionData

  /**
   * Specific class for the case of none optional data.
   */
  type NoneData <: OptionData

  /**
   * Base class with general properties of all data, independent of domain.
   */
  abstract class BaseData {
    /**
     * The type of this data.
     */
    def datatype: Type
  }

  /**
   * Base mixin for primitive data.
   */
  trait PrimitiveDataMixin[T] extends BaseData {
    this: PrimitiveData[T] =>

    def datatype: PrimitiveType[T]
    
    /**
     * The raw value of this primitive data.
     */
    def value: T
  }

  /**
   * Base mixin for tuple data.
   */
  trait TupleDataMixin extends BaseData {
    this: TupleData =>

    def datatype: TupleType
    
    /**
     * The coordinates of this tuple data.
     */
    def coordinates: IndexedSeq[Data]

    /**
     * The length (arity) of this tuple data.
     */
    def length: Int = datatype.length

    /**
     * Checks if this tuple data has a coordinate at some position.
     * Positions of optional coordinates which are not filled-in 
     * are regarded as not occupied.
     */
    def isOccupied(position: Int): Boolean = {
      datatype.hasCoordinate(position) && isFilled(coordinates(position))
    }

    /**
     * Retrieves the coordinate of this data at some position.
     */
    def coordinate(position: Int): Data = {
      require(datatype.hasCoordinate(position),
        s"Coordinate position: $position is out of bounds for TupleType: $datatype")

      coordinates(position)
    }
  }

  /**
   * Base mixin for struct data.
   */
  trait StructDataMixin extends BaseData {
    this: StructData =>

    def datatype: StructType
    
    /**
     * The features of this struct data.
     */
    def features: Map[String, Data]

    /**
     * Checks if this struct data has a feature with some name.
     * Names of optional features which are not filled-in
     * are regarded as not occupied.
     */
    def isOccupied(name: String): Boolean = {
      features.contains(name) && isFilled(features(name))
    }

    /**
     * Retrieves the feature of this struct data with some name.
     */
    def feature(name: String): Data = {
      require(datatype.hasFeature(name),
        s"StructData doesn't contain a feature named: $name")

      features(name)
    }
  }

  /**
   * Base mixin for seq data.
   */
  trait SeqDataMixin extends BaseData {
    this: SeqData =>

    def datatype: SeqType
    
    /**
     * The elements of this seq data.
     */
    def elements: Seq[Data]
    
    /**
     * Checks if this struct data has a feature with some name.
     * Names of optional features which are not filled-in
     * are regarded as not occupied.
     */
    def isOccupied(index: Int): Boolean = {
      0 <= index && index < length && isFilled(elements(index))
    }
    
    /**
     * Retrieves the element of this seq data at some index.
     */
    def element(index: Int): Data = {
      require(0 <= index && index < length,
        s"Index: $index is out of bounds for SeqData with length: $length.")

      elements(index)
    }
    
    /**
     * The length of this seq data.
     */
    def length: Int = elements.length
  }
  
  /**
   * Base mixin for OptionData.
   */
  trait OptionDataMixin extends BaseData {
    this: OptionData =>
    
    def datatype: OptionType
    
    /**
     * Signifies that this option data has some value.
     */
    def isSome: Boolean
  }

  /**
   * Base mixin for OptionData in case of none data.
   */
  trait NoneDataMixin extends OptionDataMixin {
    this: NoneData =>
    
      def isSome = false
  }

  /**
   * Base mixin for OptionData in case of some data.
   */
  trait SomeDataMixin extends OptionDataMixin {
    this: SomeData =>
    
    def isSome = true
    
    /**
     * The value of this option data.
     */
    def value: Data
  }
  
  /**
   * Checks if data contains a value.
   * OptionData in case of none data doesn't contain a value.
   */
  def isFilled(data: Data): Boolean = data match {
    case data: OptionDataMixin => data.isSome
    case _ => true
  }
}