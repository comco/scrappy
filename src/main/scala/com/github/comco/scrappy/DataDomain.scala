package com.github.comco.scrappy

/**
 * Domain for bare data values.
 */
object DataDomain extends Domain {
  sealed abstract class Data extends BaseData {
    def originatedFrom(origin: Origin): OriginatedDataDomain.Data
  }

  case class PrimitiveData[T](val value: T)(
    implicit val datatype: PrimitiveType[T])
      extends Data with BasePrimitiveData[T] {

    def originatedFrom(origin: Origin): OriginatedDataDomain.PrimitiveData[T] =
      OriginatedDataDomain.PrimitiveData(this, origin)
  }

  case class TupleData(val datatype: TupleType,
    val coordinates: IndexedSeq[Data])
      extends Data with BaseTupleData {
    require(coordinates != null, "Can't construct TupleData with null coordinates.")
    require(datatype.size == coordinates.size, 
        s"Invalid size of coordinates to construct a TupleData; expected: ${datatype.size}, actual: ${coordinates.size}.")
    require(datatype.coordinateTypes.zip(coordinates).forall {
      case (datatype, data) => data.datatype == datatype
    }, "Data coordinates types don't match tuple datatypes for TupleData construction.")
        
    def originatedFrom(origin: Origin): OriginatedDataDomain.TupleData =
      OriginatedDataDomain.OriginalTupleData(this, origin)
  }

  case class StructData(val datatype: StructType,
    val features: Map[String, Data])
      extends Data with BaseStructData {

    def originatedFrom(origin: Origin): OriginatedDataDomain.StructData =
      OriginatedDataDomain.OriginalStructData(this, origin)
  }

  case class SeqData(val datatype: SeqType,
    val elements: Seq[Data])
      extends Data with BaseSeqData {

    def originatedFrom(origin: Origin): OriginatedDataDomain.SeqData =
      OriginatedDataDomain.OriginalSeqData(this, origin)
  }
}