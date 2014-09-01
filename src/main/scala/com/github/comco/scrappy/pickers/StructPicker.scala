package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class StructPicker(val targetType: StructType, val featurePickers: Map[String, Picker])
    extends BasePicker {
  require(featurePickers.forall {
    case (name, picker) => picker.sourceType == sourceType
  }, s"All featurePickers: $featurePickers should have the same sourceType.")
  require(featurePickers.forall {
    case (name, picker) => targetType.hasFeature(name) &&
      picker.targetType == targetType.featureType(name)
  }, "A picker with invalid name or targetType is given")
  require(targetType.featureTypes.forall {
    case (name, _) => featurePickers.contains(name)
  }, "Missing feature name")
  
  def sourceType: Type = featurePickers.head._2.sourceType
  
  def doPickData(source: DataDomain.Data): DataDomain.StructData = {
    DataDomain.StructData(targetType, featurePickers.map {
      case (name, picker) => (name, picker.pickData(source))
    })
  }
  
  def doPickOriginatedData(source: OriginatedDataDomain.Data): OriginatedDataDomain.StructData = {
    val data = doPickData(source.data)
    val origin = source.origin.computedWithTargetType(targetType)
    val features = featurePickers.map {
      case (name, picker) => (name, picker.pickOriginatedData(source))
    }
    OriginatedDataDomain.ComputedStructData(data, origin, features)
  }
}

object StructPicker {
  def apply(targetType: StructType)(featurePickers: (String, Picker)*): StructPicker =
    StructPicker(targetType, featurePickers.toMap)
}