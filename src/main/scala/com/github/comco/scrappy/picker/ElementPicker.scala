package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class ElementPicker[Element <: Shape.Any](val sourceSchema: Schema.Sequence[Element], val index: Int)
    extends BasePicker[Shape.Sequence[Element], Element] {
  require(index >= 0, s"Index: $index should be non-negative.")

  override def targetSchema = sourceSchema.elementSchema

  override def doPick(source: Data.Sequence[Element]): Data[Element] =
    source.elements(index)
}