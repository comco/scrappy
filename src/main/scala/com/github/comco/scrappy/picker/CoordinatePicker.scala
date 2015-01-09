package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class CoordinatePicker(val sourceSchema: Schema.Tuple, val position: Int)
    extends BasePicker[Shape.Tuple, Shape.Any] {
  require(0 <= position, s"Position: $position should be positive.")
  require(position < sourceSchema.coordinateSchemas.length,
    s"Position: $position is invalid position for the tuple schema: $sourceSchema.")

  override def targetSchema = sourceSchema.coordinateSchemas(position)

  override def doPick(source: Data.Tuple): Data.Any =
    source.coordinates(position)
}