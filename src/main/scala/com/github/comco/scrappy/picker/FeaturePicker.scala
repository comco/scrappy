package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class FeaturePicker(val sourceSchema: Schema.Struct, val featureName: String)
    extends BasePicker[Shape.Struct, Shape.Any] {
  require(sourceSchema.featureSchemas.contains(featureName),
    s"Struct schema: $sourceSchema doesn't contain a feature named: $featureName.")

  override def targetSchema = sourceSchema.featureSchemas(featureName)

  override def doPick(source: Data.Struct): Data.Any =
    source.features(featureName)
}