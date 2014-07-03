package scrappy

sealed abstract class Type

case object IntType extends Type

case object StringType extends Type

case object BooleanType extends Type

case class SeqType(val elementType: Type) extends Type

case class TupleType(val coordinateTypes: IndexedSeq[Type]) extends Type

case class StructType(val name: String, val featureTypes: Map[String, Type])
  extends Type