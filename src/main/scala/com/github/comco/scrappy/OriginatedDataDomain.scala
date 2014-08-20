package com.github.comco.scrappy

/**
 * Domain for data values with origin.
 */
object OriginatedDataDomain extends Domain {
  def mkDataOriginatedFrom(data: DataDomain.Data, origin: Origin): Data = ???
  
  sealed abstract class Data extends BaseData {
    def data: DataDomain.Data
    def origin: Origin
  }

  case class PrimitiveData[T](val data: DataDomain.PrimitiveData[T],
    val origin: Origin)
      extends Data with BasePrimitiveData[T] {
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
    lazy val coordinates: IndexedSeq[Data] =
      data.coordinates.zipWithIndex.map {
        case (coord, pos) => mkDataOriginatedFrom(coord,
          origin.append(CoordinateStep(datatype, pos)))
      }
  }

  case class ComputedTupleData(val data: DataDomain.TupleData,
    val origin: Origin,
    val coordinates: IndexedSeq[Data])
      extends TupleData

  sealed abstract class StructData extends Data with BaseStructData {
    def data: DataDomain.StructData
    def datatype: StructType = data.datatype
  }

  case class OriginalStructData(val data: DataDomain.StructData,
    val origin: Origin)
      extends StructData {

    lazy val features: Map[String, Data] = data.features.map {
      case (name, feature) => (name, mkDataOriginatedFrom(feature,
        origin.append(FeatureStep(datatype, name))))
    }
  }

  case class ComputedStructData(val data: DataDomain.StructData,
    val origin: Origin, val features: Map[String, Data])
      extends StructData

  sealed abstract class SeqData extends Data with BaseSeqData {
    def data: DataDomain.SeqData
    def datatype: SeqType = data.datatype
  }

  case class OriginalSeqData(val data: DataDomain.SeqData,
    val origin: Origin)
      extends SeqData {

    lazy val elements: Seq[Data] = data.elements.zipWithIndex.map {
      case (elem, index) => mkDataOriginatedFrom(elem,
        origin.append(ElementStep(datatype, index)))
    }
  }

  case class ComputedSeqData(val data: DataDomain.SeqData,
    val origin: Origin,
    val elements: Seq[Data])
      extends SeqData
}