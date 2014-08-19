package com.github.comco.scrappy

import scala.language.implicitConversions

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

  object PrimitiveData {
    implicit def raw2PrimitiveData[T](value: T)(
        implicit datatype: PrimitiveType[T]): PrimitiveData[T] = {
      PrimitiveData(value)
    }
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

  object TupleData {
    def apply(datatype: TupleType)(coordinates: Data*): TupleData = {
      TupleData(datatype, coordinates.toIndexedSeq)
    }
  }

  case class StructData(val datatype: StructType,
    val features: Map[String, Data])
      extends Data with BaseStructData {
    require(features != null, "StructData cannot be constructed with null features")
    require(features.keys.forall(datatype.hasFeature(_)), "Invalid feature name")
    require(features.forall {
      case (name, data) if data != null => data.datatype == datatype.featureType(name)
      case _ => true
    }, "Invalid feature type")
    
    def originatedFrom(origin: Origin): OriginatedDataDomain.StructData =
      OriginatedDataDomain.OriginalStructData(this, origin)
  }
  
  object StructData {
    def apply(datatype: StructType)(features: (String, Data)*): StructData = {
      StructData(datatype, features.toMap)
    }
  }

  case class SeqData(val datatype: SeqType,
    val elements: Seq[Data])
      extends Data with BaseSeqData {
    require(elements != null, "SeqData cannot be constructed with null elements")
    require(elements.forall {
      case element if element != null => element.datatype == datatype.elementType
      case _ => true
    }, "An element has invalid datatype")
    
    def originatedFrom(origin: Origin): OriginatedDataDomain.SeqData =
      OriginatedDataDomain.OriginalSeqData(this, origin)
  }
  
  object SeqData {
    case class Dummy(private[SeqData] val real: SeqData)
    
    def apply(datatype: SeqType)(elements: Data*): Dummy = {
      Dummy(SeqData(datatype, elements))
    }
    
    implicit def Dummy2SeqData(dummy: Dummy): SeqData = dummy.real
  }
}