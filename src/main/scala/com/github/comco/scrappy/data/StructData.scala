package com.github.comco.scrappy.data

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.simple.SimpleStructData

abstract class StructData extends Data.Base {
  def datatype: StructType

  /**
   * The features of this struct data.
   */
  def features: Map[String, Data]

  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(name: String): Boolean = {
    features.contains(name) && Data.isFilled(features(name))
  }

  /**
   * Retrieves the feature of this struct data with some name.
   */
  def feature(name: String): Data = {
    require(datatype.hasFeature(name),
      s"StructData doesn't contain a feature named: $name")

    features(name)
  }
}

abstract class BaseStructDataCompanionObject {
  def apply(datatype: StructType, rawFeatures: Map[String, Data]): StructData = {
    require(rawFeatures.keys.forall(datatype.hasFeature(_)), "Invalid feature name")
    require(rawFeatures.forall {
      case (name, data) => {
        Data.canAssign(datatype.featureType(name), data)
      }
    }, s"Invalid feature type for creating a StructData with datatype: $datatype from features: $rawFeatures")
    require(datatype.featureTypes.forall {
      case (name, datatype) => rawFeatures.contains(name) || datatype.isInstanceOf[OptionType]
    }, "A non-optional feature is not given")

    val features: Map[String, Data] = {
      val convertedFeatures: Map[String, Data] = rawFeatures.transform({
        case (name, data) => Data.convert(datatype.featureType(name), data)
      })
      val missingFeatures: Map[String, Type] = datatype.featureTypes.filter({
        case (name, _) => !rawFeatures.contains(name)
      })
      // all missing features should have option types
      convertedFeatures ++ missingFeatures.map {
        case (name, datatype) => (name, NoneData(datatype.asInstanceOf[OptionType]))
      }
    }
    doApply(datatype, features)
  }

  def doApply(datatype: StructType, features: Map[String, Data]): StructData
}

object StructData extends BaseStructDataCompanionObject {
  def doApply(datatype: StructType, features: Map[String, Data]): StructData =
    SimpleStructData(datatype, features)

  def apply(datatype: StructType)(features: (String, Data)*): StructData = {
    StructData(datatype, features.toMap)
  }
}