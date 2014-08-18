package com.github.comco.scrappy

object DataDomain extends Domain {
  sealed abstract class Data extends BaseData {
    def withOrigin(origin: Origin): DataWithOriginDomain.Data
  }

  case class PrimitiveData[T](val data: T)(
    implicit val datatype: PrimitiveType[T])
      extends Data with BasePrimitiveData[T] {

    def withOrigin(origin: Origin): DataWithOriginDomain.PrimitiveData[T] =
      DataWithOriginDomain.PrimitiveData(this, origin)
  }

  case class TupleData(val datatype: TupleType,
    val coordinates: IndexedSeq[Data])
      extends Data with BaseTupleData {

    def withOrigin(origin: Origin): DataWithOriginDomain.TupleData =
      DataWithOriginDomain.OriginalTupleData(this, origin)
  }

  case class StructData(val datatype: StructType,
    val features: Map[String, Data])
      extends Data with BaseStructData {

    def withOrigin(origin: Origin): DataWithOriginDomain.StructData =
      DataWithOriginDomain.OriginalStructData(this, origin)
  }

  case class SeqData(val datatype: SeqType,
    val elements: Seq[Data])
      extends Data with BaseSeqData {

    def withOrigin(origin: Origin): DataWithOriginDomain.SeqData =
      DataWithOriginDomain.OriginalSeqData(this, origin)
  }
}