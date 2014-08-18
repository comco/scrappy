package com.github.comco.scrappy

sealed abstract class Type

sealed abstract class PrimitiveType[T] extends Type

case class TupleType(val coordinateTypes: IndexedSeq[Type])
    extends Type {
  
  def hasCoordinate(position: Int) = 
    (0 <= position && position < coordinateTypes.length)
  
  def coordinateType(position: Int) = coordinateTypes(position)
}

case class StructType(val name: String, val featureTypes: Map[String, Type])
    extends Type {
  
  def hasFeature(name: String) = featureTypes.contains(name)
  
  def featureType(name: String) = featureTypes(name)
}

case class SeqType(val elementType: Type) extends Type

object Type {
  implicit case object IntPrimitiveType extends PrimitiveType[Int]
  implicit case object StringPrimitiveType extends PrimitiveType[String]
  implicit case object BooleanPrimitiveType extends PrimitiveType[Boolean]
}