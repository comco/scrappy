package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

/**
 * Picker for a featur of a struct.
 */
case class FeaturePicker(val sourceType: Type.Struct, val name: String)
    extends Picker[Shape.Struct, Shape.Any] {
  require(sourceType.hasFeature(name))

  def targetType = sourceType.featureType(name)

  def pickData(source: Data.Struct) = source.features(name)
  def pickOriginatedData(source: OriginatedData.Struct) = source.features(name)
}