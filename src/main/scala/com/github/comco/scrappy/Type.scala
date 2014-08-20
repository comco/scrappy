package com.github.comco.scrappy

/**
 * Base class for scrappy types.
 */
sealed abstract class Type

/**
 * Base class for the primitive scrappy types.
 * These are predefined - user-defined types cannot be primitive.
 */
sealed abstract class PrimitiveType[T] extends Type

/**
 * Tuple type is for data having coordinates. The coordinates can be indexed by position.
 * The position is zero-based.
 */
case class TupleType(val coordinateTypes: IndexedSeq[Type])
    extends Type {
  
  def size: Int = coordinateTypes.size
  
  def hasCoordinate(position: Int): Boolean =
    (0 <= position && position < coordinateTypes.length)

  def coordinateType(position: Int): Type = {
    require(hasCoordinate(position), s"Invalid coordinate position: $position for a TupleType: $this.")
    coordinateTypes(position)
  }
}

object TupleType {
  def apply(coordinatesTypes: Type*): TupleType = TupleType(coordinatesTypes.toIndexedSeq)
}

/**
 * A struct type has a name named features. The name of the feature is used as a key for identifying that
 * feature in the struct.
 */
case class StructType(val name: String, val featureTypes: Map[String, Type])
    extends Type {
  
  def size: Int = featureTypes.size
  
  def hasFeature(name: String): Boolean = featureTypes.contains(name)

  def featureType(name: String): Type = {
    require(hasFeature(name), s"Invalid feature name: $name for a StructType: $this.")
    featureTypes(name)
  }
}

object StructType {
  def apply(name: String, featureTypes: (String, Type)*): StructType = {
    StructType(name, featureTypes.toMap)
  }
}

/**
 * A type for representing a sequence of elements of the same type.
 */
case class SeqType(val elementType: Type) extends Type

case class OptionType(val someType: Type) extends Type {
  require(!someType.isInstanceOf[OptionType], 
    s"OptionType is flat; it should have a concrete (non-optional) someType. Passed someType: $someType")
}

object PrimitiveType {
  implicit case object IntPrimitiveType extends PrimitiveType[Int]
  implicit case object StringPrimitiveType extends PrimitiveType[String]
  implicit case object BooleanPrimitiveType extends PrimitiveType[Boolean]
}