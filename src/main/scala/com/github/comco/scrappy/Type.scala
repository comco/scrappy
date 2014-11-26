package com.github.comco.scrappy

import scala.reflect.runtime.universe._
import scala.language.implicitConversions

/**
 * Base class for scrappy types.
 *
 * Types have a lattice structure.
 */
sealed abstract class Type[+Shape <: Shape.Any: TypeTag] {
  val shape = typeOf[Shape]

  /**
   * Defines a subtype relation.
   */
  def <:<(that: Type.Any): Boolean = {
    this.shape <:< that.shape
  }
}

object Type {
  type Any = Type[Shape.Any]
  implicit final object Any extends Any

  type Nil = Type[Shape.Nil]
  implicit final object Nil extends Nil

  type Primitive[+RawType] = Type[Shape.Primitive[RawType]]
  implicit def toPrimitiveType[RawType](tpe: Primitive[RawType]) =
    tpe.asInstanceOf[PrimitiveType[RawType]]

  type Tuple = Type[Shape.Tuple]
  implicit def toTupleType(tpe: Tuple) = tpe.asInstanceOf[TupleType]

  type Struct = Type[Shape.Struct]
  implicit def toStructType(tpe: Struct) = tpe.asInstanceOf[StructType]

  type Seq[+ElementShape <: Shape.Any] = Type[Shape.Seq[ElementShape]]
  implicit def toSeqType[ElementShape <: Shape.Any](tpe: Seq[ElementShape]) =
    tpe.asInstanceOf[SeqType[ElementShape]]

  type Optional[+ValueShape <: Shape.Concrete] = Type[Shape.Optional[ValueShape]]
  implicit def toOptionalType[ValueShape <: Shape.Concrete](tpe: Optional[ValueShape]) =
    tpe.asInstanceOf[OptionalType[ValueShape]]
}

/**
 * Base class for primitive scrappy types.
 * These are predefined - user-defined types cannot be primitive.
 */
sealed abstract class PrimitiveType[+T: TypeTag] extends Type.Primitive[T]

/**
 * Tuple type is for data having coordinates. The coordinates can be indexed by
 * position. The position is zero-based.
 */
case class TupleType(val coordinateTypes: IndexedSeq[Type.Any])
    extends Type[Shape.Tuple] {

  /**
   * The number of coordinates of this tuple type.
   */
  def length: Int = coordinateTypes.length

  /**
   * Checks if this tuple type contains a coordinate at some position.
   */
  def hasCoordinate(position: Int): Boolean =
    (0 <= position && position < coordinateTypes.length)

  /**
   * Retrieves the type of the coordinate of this tuple type at some position.
   */
  def coordinateType(position: Int): Type.Any = {
    require(hasCoordinate(position),
      s"Invalid coordinate position: $position for a TupleType: $this.")

    coordinateTypes(position)
  }
}

object TupleType {
  /**
   * Constructs a tuple type from a series of types representing the coordinate
   * types.
   */
  def apply(coordinatesTypes: Type.Any*): TupleType =
    TupleType(coordinatesTypes.toIndexedSeq)
}

/**
 * A struct type has a name named features. The name of the feature is used as
 * a key for identifying that feature in the struct.
 */
case class StructType(val name: String, val featureTypes: Map[String, Type.Any])
    extends Type[Shape.Struct] {

  /**
   * The size (number of features) of this struct type.
   */
  def size: Int = featureTypes.size

  /**
   * Checks if this struct type has a feature with some name.
   */
  def hasFeature(name: String): Boolean = featureTypes.contains(name)

  /**
   * Retrieves the feature type of a feature with some name from this struct
   * type.
   */
  def featureType(name: String): Type.Any = {
    require(hasFeature(name),
      s"Invalid feature name: $name for a StructType: $this.")

    featureTypes(name)
  }
}

object StructType {
  /**
   * Constructs a struct type with some name and a sequence of named feature
   * types.
   */
  def apply(name: String, featureTypes: (String, Type.Any)*): StructType = {
    StructType(name, featureTypes.toMap)
  }
}

/**
 * A type for representing a sequence of elements of the same type.
 */
case class SeqType[+ElementShape <: Shape.Any: TypeTag](val elementType: Type[ElementShape])
  extends Type.Seq[ElementShape]

/**
 * A type for optional values. An optional value could either be none or some,
 * and the type of the value must be a concrete (non-optional) type.
 * Thus optional values are flat - you cannot have an optional value inside
 * an optional value.
 */
case class OptionalType[+ValueShape <: Shape.Concrete: TypeTag](val valueType: Type[ValueShape])
  extends Type.Optional[ValueShape]