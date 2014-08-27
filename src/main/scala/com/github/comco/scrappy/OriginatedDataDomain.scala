package com.github.comco.scrappy

/**
 * Domain for data values with origin.
 */
object OriginatedDataDomain extends Domain {
  def mkDataOriginatedFrom(data: DataDomain.Data, origin: Origin): OriginatedDataDomain.Data = {
    require(data.datatype == origin.targetType)
    data match {
      case data: DataDomain.PrimitiveData[t] => PrimitiveData[t](data, origin)
      case data: DataDomain.TupleData => OriginalTupleData(data, origin)
      case data: DataDomain.StructData => OriginalStructData(data, origin)
      case data: DataDomain.SeqData => OriginalSeqData(data, origin)
      case data: DataDomain.SomeData => OriginalSomeData(data, origin)
      case data: DataDomain.NoneData => NoneData(data, origin)
    }
  }

  sealed abstract class Data extends BaseData {
    def data: DataDomain.Data
    def origin: Origin
  }

  case class PrimitiveData[T](val data: DataDomain.PrimitiveData[T],
    val origin: Origin)
      extends Data with BasePrimitiveData[T] {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")
    def datatype: PrimitiveType[T] = data.datatype
    def value = data.value
  }

  sealed abstract class TupleData extends Data with BaseTupleData {
    def data: DataDomain.TupleData
    def datatype: TupleType = data.datatype
  }

  case class OriginalTupleData(val data: DataDomain.TupleData,
    val origin: Origin)
      extends TupleData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")
    lazy val coordinates: IndexedSeq[Data] =
      data.coordinates.zipWithIndex.map {
        case (coord, pos) => mkDataOriginatedFrom(coord,
          origin.append(CoordinateStep(datatype, pos)))
      }
  }

  case class ComputedTupleData(val data: DataDomain.TupleData,
    val origin: Origin,
    val coordinates: IndexedSeq[Data])
      extends TupleData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")
  }

  sealed abstract class StructData extends Data with BaseStructData {
    def data: DataDomain.StructData
    def datatype: StructType = data.datatype
  }

  case class OriginalStructData(val data: DataDomain.StructData,
    val origin: Origin)
      extends StructData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")

    lazy val features: Map[String, Data] = data.features.map {
      case (name, feature) => (name, mkDataOriginatedFrom(feature,
        origin.append(FeatureStep(datatype, name))))
    }
  }

  case class ComputedStructData(val data: DataDomain.StructData,
    val origin: Origin, val features: Map[String, Data])
      extends StructData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")
  }

  sealed abstract class SeqData extends Data with BaseSeqData {
    def data: DataDomain.SeqData
    def datatype: SeqType = data.datatype
  }

  case class OriginalSeqData(val data: DataDomain.SeqData,
    val origin: Origin)
      extends SeqData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")

    lazy val elements: Seq[Data] = data.elements.zipWithIndex.map {
      case (elem, index) => mkDataOriginatedFrom(elem,
        origin.append(ElementStep(datatype, index)))
    }
  }

  case class ComputedSeqData(val data: DataDomain.SeqData,
    val origin: Origin,
    val elements: Seq[Data])
      extends SeqData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")
  }

  sealed abstract class OptionData extends Data with BaseOptionData {
    def data: DataDomain.OptionData
    def datatype: OptionType = data.datatype
  }

  case class NoneData(val data: DataDomain.NoneData, val origin: Origin)
      extends OptionData with BaseNoneData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")
  }

  sealed abstract class SomeData extends OptionData with BaseSomeData

  case class OriginalSomeData(val data: DataDomain.SomeData, val origin: Origin)
      extends SomeData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")

    lazy val value: Data = mkDataOriginatedFrom(data.value, origin.append(SomeStep(datatype)))
  }

  case class ComputedSomeData(val data: DataDomain.SomeData, val origin: Origin, val value: Data)
      extends SomeData {
    require(data.datatype == origin.targetType,
      s"Datatype of data: $data and targetType of origin: $origin does not match.")
  }
}