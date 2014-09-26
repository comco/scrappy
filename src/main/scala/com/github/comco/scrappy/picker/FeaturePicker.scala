package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.originated_data.OriginatedStructData

/**
 * Picker for a featur of a struct.
 */
case class FeaturePicker(val sourceType: StructType, val name: String)
    extends BaseStructPicker {
  require(sourceType.hasFeature(name))
  
  def targetType = sourceType.featureType(name)
  
  def doPickData(source: StructData) = source.features(name)
  def doPickOriginatedData(source: OriginatedStructData) = source.features(name)
}