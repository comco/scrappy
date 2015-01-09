package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class MapPicker[-Source <: Shape.Any, +Target <: Shape.Any](
    val f: Picker[Source, Target])(
        implicit schemaFactory: Schema.Factory, dataFactory: Data.Factory)
    extends BasePicker[Shape.Sequence[Source], Shape.Sequence[Target]] {
  override def sourceSchema = Schema.Sequence(f.sourceSchema)
  
  override def targetSchema = Schema.Sequence(f.targetSchema)
  
  override def doPick(source: Data.Sequence[Source]): Data.Sequence[Target] = {
    val targetElements = source.elements.map(f.pick(_))
    val targetOrigin = source.origin.computed
    Data.Sequence(targetElements)(targetOrigin, targetSchema)
  }
}