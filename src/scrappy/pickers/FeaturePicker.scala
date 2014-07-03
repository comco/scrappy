package scrappy.pickers

import scrappy.StructType
import scrappy.Picker
import scrappy.Value
import scrappy.StructValue
import scrappy.ValueWithOrigin
import scrappy.ValueWithOrigin
import scrappy.Original
import scrappy.ValueWithOrigin
import scrappy.FeaturePointer
import scrappy.ValueWithOrigin


case class FeaturePicker(val sourceType: StructType, val name: String)
extends Picker {
  require(sourceType.featureTypes.contains(name))
  
  val resultType = sourceType.featureTypes(name)
  
  protected def doPick(source: Value): Value = (source: @unchecked) match {
    case StructValue(_, features) => features(name)
  }
}