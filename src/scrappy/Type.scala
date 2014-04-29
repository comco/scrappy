package scrappy

sealed trait Type {
  def matches(that: Type) =
    this == that || this == AnyType || that == AnyType
}

case object AnyType extends Type

case class PrimitiveType[T]() extends Type

case class TupleType(val coordinateTypes: IndexedSeq[Type]) extends Type {
  def apply(coordinates: Values.Data*) = 
    Values.TupleData(this, coordinates.toIndexedSeq)
}

object TupleType {
  def apply(coordinateTypes: Type*) = new TupleType(coordinateTypes.toIndexedSeq)
}

case class StructType(val name: String, val featureTypes: Map[String, Type])
  extends Type {
  def apply(features: (String, Values.Data)*) =
    Values.StructData(this, Map(features : _*))
}

object StructType {
  def apply(name: String, featureTypes: (String, Type)*) =
    new StructType(name, Map(featureTypes: _*))
}
  
case class SeqType(val elementType: Type) extends Type {
  def apply(elements: Values.Data*) = Values.SeqData(this, elements)
}