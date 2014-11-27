package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Shape

/**
 * Picker for a featur of a struct.
 */
case class FeaturePicker(val sourceType: StructType, val name: String)
    extends Picker[Shape.Struct, Shape.Any] {
  require(sourceType.hasFeature(name))

  def targetType = sourceType.featureType(name)

  def doPickData(source: Data.Struct) = source.features(name)
  def doPickOriginatedData(source: OriginatedData.Struct) = source.features(name)
}