package scrappy

object Values extends Domain {
  sealed trait Data extends DomainData
  
  case class PrimitiveData[T](val data: T) extends Data {
    def datatype = PrimitiveType[T]
  }
  
  case class TupleData(val datatype: TupleType, val coordinates: IndexedSeq[Data]) 
    extends Data
  
  case class StructData(val datatype: StructType, val features: Map[String, Data])
    extends Data
  
  case class SeqData(val datatype: SeqType, val elements: Seq[Data])
    extends Data
  
  def select(pick: Picker, data: Data): Data = ???
} 