package com.github.comco.scrappy.picker

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.BotType

case class StructPicker[ST](val targetType: StructType,
  val featurePickers: Map[String, Picker[Type[ST], Type[Nothing]]])
    extends BasePicker[Type[ST], StructType] {
  require(featurePickers.forall {
    case (name, picker) => sourceType.isSubtypeOf(picker.sourceType)
  }, s"All featurePickers: $featurePickers should have the same sourceType.")
  require(featurePickers.forall {
    case (name, picker) => targetType.hasFeature(name) &&
      picker.targetType == targetType.featureType(name)
  }, "A picker with invalid name or targetType is given")
  require(targetType.featureTypes.forall {
    case (name, _) => featurePickers.contains(name)
  }, "Missing feature name")

  lazy val sourceType: Type[ST] = {
    val t = Type.meet(featurePickers.values.map(_.sourceType).toSeq)
    require(t != BotType, "Feature types are incompatible")
    t.asInstanceOf[Type[ST]]
  }

  def doPickData(source: Data[Type[ST]]): StructData = {
    StructData(targetType, featurePickers.map {
      case (name, picker) => (name, picker.pickData(source))
    })
  }

  def doPickOriginatedData(source: OriginatedData[Type[ST]]): OriginatedStructData = {
    val data = doPickData(source.data)
    val origin = source.origin.computedWithTargetType(targetType)
    val features = featurePickers.map {
      case (name, picker) => (name, picker.pickOriginatedData(source))
    }
    OriginatedStructData.computed(data, origin, features)
  }
}

object StructPicker {
  def apply[ST](targetType: StructType)(featurePickers: (String, Picker[Type[ST], Type[Nothing]])*): StructPicker[ST] =
    StructPicker(targetType, featurePickers.toMap)
}