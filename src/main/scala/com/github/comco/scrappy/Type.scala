package com.github.comco.scrappy

/**
 * Base class for scrappy types.
 */
sealed abstract class Type

/**
 * Base (type)class for primitive scrappy types.
 * These are predefined - user-defined types cannot be primitive.
 */
sealed abstract class PrimitiveType[T] extends Type {
  def typeName: String
}

/**
 * Tuple type is for data having coordinates. The coordinates can be indexed by
 * position. The position is zero-based.
 */
case class TupleType(val coordinateTypes: IndexedSeq[Type])
    extends Type {

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
  def coordinateType(position: Int): Type = {
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
  def apply(coordinatesTypes: Type*): TupleType =
    TupleType(coordinatesTypes.toIndexedSeq)
}

/**
 * A struct type has a name named features. The name of the feature is used as
 * a key for identifying that feature in the struct.
 */
case class StructType(val name: String, val featureTypes: Map[String, Type])
    extends Type {

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
  def featureType(name: String): Type = {
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
  def apply(name: String, featureTypes: (String, Type)*): StructType = {
    StructType(name, featureTypes.toMap)
  }
}

/**
 * A type for representing a sequence of elements of the same type.
 */
case class SeqType(val elementType: Type) extends Type

/**
 * A type for optional values. An optional value could either be none or some,
 * and the type of the value must be non-optional type.
 */
case class OptionType(val someType: Type) extends Type {
  require(!someType.isInstanceOf[OptionType],
    s"OptionType should have a non-optional someType instead of: $someType")
}

object PrimitiveType {
  implicit case object IntPrimitiveType extends PrimitiveType[Int] {
    final val typeName = "int"
  }

  implicit case object StringPrimitiveType extends PrimitiveType[String] {
    final val typeName = "string"
  }

  implicit case object BooleanPrimitiveType extends PrimitiveType[Boolean] {
    final val typeName = "boolean"
  }

  val typeNames = Map[String, PrimitiveType[_]](
    "int" -> IntPrimitiveType,
    "string" -> StringPrimitiveType,
    "boolean" -> BooleanPrimitiveType)
}