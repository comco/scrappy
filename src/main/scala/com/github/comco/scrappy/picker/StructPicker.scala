package com.github.comco.scrappy.picker

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.originated_data.OriginatedStructData

case class StructPicker[SourceType >: Type.Nil <: Type.Any](
  val targetType: StructType,
  val featurePickers: Map[String, Picker[SourceType, Type.Any]])
    extends BasePicker[SourceType, StructType] {

  require(featurePickers.forall {
    case (name, picker) => sourceType <:< picker.sourceType
  }, s"All featurePickers: $featurePickers should have the same sourceType.")
  require(featurePickers.forall {
    case (name, picker) => targetType.hasFeature(name) &&
      picker.targetType == targetType.featureType(name)
  }, "A picker with invalid name or targetType is given")
  require(targetType.featureTypes.forall {
    case (name, _) => featurePickers.contains(name)
  }, "Missing feature name")

  lazy val sourceType: SourceType = {
    ??? // TODO
    //    val t = Type.meet(featurePickers.values.map(_.sourceType).toSeq)
    //    require(t != BotType, "Feature types are incompatible")
    //    t.asInstanceOf[Type[ST]]
  }

  def doPickData(source: Data[SourceType]): StructData = {
    StructData(targetType, featurePickers.map {
      case (name, picker) => (name, picker.pickData(source))
    })
  }

  def doPickOriginatedData(source: OriginatedData[SourceType]): OriginatedStructData = {
    val data = doPickData(source.data)
    val origin = source.origin.computedWithTargetType(targetType)
    val features = featurePickers.map {
      case (name, picker) => (name, picker.pickOriginatedData(source))
    }
    OriginatedStructData.computed(data, origin, features)
  }
}

object StructPicker {
  def apply[SourceType >: Type.Nil <: Type.Any](
    targetType: StructType)(
      featurePickers: (String, Picker[SourceType, Type.Any])*): StructPicker[SourceType] =
    StructPicker(targetType, featurePickers.toMap)
}