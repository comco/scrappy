package com.github.comco.scrappy

sealed abstract class Type

sealed abstract class PrimitiveType[T] extends Type

case class TupleType(val coordinateTypes: IndexedSeq[Type])
    extends Type {
  
  def size: Int = coordinateTypes.size
  
  def hasCoordinate(position: Int) =
    (0 <= position && position < coordinateTypes.length)

  def coordinateType(position: Int) = {
    require(hasCoordinate(position), s"Invalid coordinate position: $position for a TupleType: $this")
    coordinateTypes(position)
  }
}

object TupleType {
  def apply(coordinatesTypes: Type*): TupleType = TupleType(coordinatesTypes.toIndexedSeq)
}

case class StructType(val name: String, val featureTypes: Map[String, Type])
    extends Type {
  
  def size: Int = featureTypes.size
  
  def hasFeature(name: String) = featureTypes.contains(name)

  def featureType(name: String) = {
    require(hasFeature(name), s"Invalid feature name: $name for a StructType: $this")
    featureTypes(name)
  }
}

object StructType {
  def apply(name: String, featureTypes: (String, Type)*): StructType = {
    StructType(name, featureTypes.toMap)
  }
}

case class SeqType(val elementType: Type) extends Type

object PrimitiveType {
  implicit case object IntPrimitiveType extends PrimitiveType[Int]
  implicit case object StringPrimitiveType extends PrimitiveType[String]
  implicit case object BooleanPrimitiveType extends PrimitiveType[Boolean]
}