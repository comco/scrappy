package com.github.comco.scrappy

/**
 * Domain for bare data values.
 */
object DataDomain extends Domain {
  sealed abstract class Data extends BaseData {
    def withOrigin(origin: Origin): OriginatedDataDomain.Data
  }

  case class PrimitiveData[T](val data: T)(
    implicit val datatype: PrimitiveType[T])
      extends Data with BasePrimitiveData[T] {

    def withOrigin(origin: Origin): OriginatedDataDomain.PrimitiveData[T] =
      OriginatedDataDomain.PrimitiveData(this, origin)
  }

  case class TupleData(val datatype: TupleType,
    val coordinates: IndexedSeq[Data])
      extends Data with BaseTupleData {

    def withOrigin(origin: Origin): OriginatedDataDomain.TupleData =
      OriginatedDataDomain.OriginalTupleData(this, origin)
  }

  case class StructData(val datatype: StructType,
    val features: Map[String, Data])
      extends Data with BaseStructData {

    def withOrigin(origin: Origin): OriginatedDataDomain.StructData =
      OriginatedDataDomain.OriginalStructData(this, origin)
  }

  case class SeqData(val datatype: SeqType,
    val elements: Seq[Data])
      extends Data with BaseSeqData {

    def withOrigin(origin: Origin): OriginatedDataDomain.SeqData =
      OriginatedDataDomain.OriginalSeqData(this, origin)
  }
}