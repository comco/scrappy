package scrappy

sealed abstract class Value {
  def datatype: Type
}

case class IntValue(val data: Int) extends Value {
  val datatype = IntType
}

case class StringValue(val data: String) extends Value {
  val datatype = StringType
}

case class BooleanValue(val data: Boolean) extends Value {
  val datatype = BooleanType
}

case class SeqValue(val datatype: SeqType, val elements: Seq[Value])
  extends Value

case class TupleValue(val datatype: TupleType, val coordinates: IndexedSeq[Value])
  extends Value

case class StructValue(val datatype: StructType, val features: Map[String, Value])
  extends Value

case class ValueWithOrigin(val value: Value, val origin: Origin)