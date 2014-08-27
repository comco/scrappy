package com.github.comco.scrappy

import scala.language.implicitConversions

/**
 * Domain for bare data values.
 */
object DataDomain extends Domain {
  sealed abstract class Data extends BaseData

  case class PrimitiveData[T](val value: T)(
    implicit val datatype: PrimitiveType[T])
      extends Data with BasePrimitiveData[T]

  object PrimitiveData {
    implicit def raw2PrimitiveData[T](value: T)(
        implicit datatype: PrimitiveType[T]): PrimitiveData[T] = {
      PrimitiveData(value)
    }
  }

  case class TupleData(val datatype: TupleType,
    val coordinates: IndexedSeq[Data])
      extends Data with BaseTupleData {
    require(datatype.size == coordinates.size,
      s"Invalid size of coordinates to construct a TupleData; expected: ${datatype.size}, actual: ${coordinates.size}.")
    require(datatype.coordinateTypes.zip(coordinates).forall {
      case (datatype, data) => data.datatype == datatype
    }, "Data coordinates types don't match tuple datatypes for TupleData construction.")
  }

  object TupleData {
    def apply(datatype: TupleType)(coordinates: Data*): TupleData = {
      TupleData(datatype, coordinates.toIndexedSeq)
    }
    
    def apply(coordinates: Data*): TupleData = {
      val datatype = TupleType(coordinates.map(_.datatype).toIndexedSeq)
      TupleData(datatype, coordinates.toIndexedSeq)
    }
  }

  case class StructData(val datatype: StructType,
    rawFeatures: Map[String, Data])
      extends Data with BaseStructData {
    require(rawFeatures.keys.forall(datatype.hasFeature(_)), "Invalid feature name")
    require(rawFeatures.forall {
      case (name, data) => {
        canAssign(datatype.featureType(name), data)
      }
    }, s"Invalid feature type for creating a StructData with datatype: $datatype from features: $rawFeatures")
    require(datatype.featureTypes.forall {
      case (name, datatype) => rawFeatures.contains(name) || datatype.isInstanceOf[OptionType]
    }, "A non-optional feature is not given")
    
    lazy val features: Map[String, Data] = {
      val convertedFeatures: Map[String, Data] = rawFeatures.transform ({
        case (name, data) => convert(datatype.featureType(name), data)
      })
      val missingFeatures: Map[String, Type] = datatype.featureTypes.filter({
        case (name, _) => !rawFeatures.contains(name)
      })
      // all missing features should have option types
      convertedFeatures ++ missingFeatures.map {
        case (name, datatype) => (name, NoneData(datatype.asInstanceOf[OptionType]))
      }
    }
  }
  
  object StructData {
    def apply(datatype: StructType)(features: (String, Data)*): StructData = {
      StructData(datatype, features.toMap)
    }
  }

  case class SeqData(val datatype: SeqType,
    rawElements: Seq[Data])
      extends Data with BaseSeqData {
	  require(rawElements.forall(canAssign(datatype.elementType, _)), "An element has invalid datatype")
    lazy val elements = rawElements.map(convert(datatype.elementType, _))
  }
  
  object SeqData {
    def apply(datatype: SeqType)(elements: Data*)(implicit dummy: DummyImplicit): SeqData = {
      SeqData(datatype, elements)
    }
    
    def apply(firstElement: Data, nextElements: Data*): SeqData = {
      SeqData(SeqType(firstElement.datatype), firstElement +: nextElements.toSeq)
    }
  }
  
  sealed abstract class OptionData extends Data with BaseOptionData
  
  case class SomeData(val datatype: OptionType, val value: Data) extends OptionData with BaseSomeData {
    require(value.datatype == datatype.someType, s"SomeData: $this cannot be created with value of type: ${value.datatype}")
  }
  
  object SomeData {
    def apply[T](value: T)(implicit datatype: PrimitiveType[T]): SomeData = {
      import PrimitiveData.raw2PrimitiveData
      SomeData(OptionType(datatype), value)
    }
  }
  
  case class NoneData(val datatype: OptionType) extends OptionData with BaseNoneData

  private def canAssign(datatype: Type, data: Data): Boolean = {
    data.datatype == datatype ||
      (datatype.isInstanceOf[OptionType] && datatype.asInstanceOf[OptionType].someType == data.datatype)
  }
  
  private def convert(datatype: Type, data: Data): Data = {
    assert(canAssign(datatype, data))
    if (data.datatype == datatype) {
      data
    } else {
      SomeData(datatype.asInstanceOf[OptionType], data)
    }
  }
}