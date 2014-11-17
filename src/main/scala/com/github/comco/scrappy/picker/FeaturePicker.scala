package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Picker for a featur of a struct.
 */
case class FeaturePicker(val sourceType: StructType, val name: String)
    extends Picker[StructType, Type[Any]] {
  require(sourceType.hasFeature(name))

  def targetType = sourceType.featureType(name)

  def doPickData(source: Data[StructType]) = source.features(name)
  def doPickOriginatedData(source: OriginatedData[StructType]) = source.features(name)
}