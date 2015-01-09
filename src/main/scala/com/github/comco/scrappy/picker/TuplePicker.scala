package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class TuplePicker[-Source <: Shape.Any](
  val coordinatePickers: IndexedSeq[Picker[Source, Shape.Any]])(
    implicit schemaFactory: Schema.Factory, dataFactory: Data.Factory)
    extends BasePicker[Source, Shape.Tuple] {

  override def sourceSchema = coordinatePickers.head.sourceSchema

  override def targetSchema = Schema.Tuple(coordinatePickers.map(_.targetSchema))

  override def doPick(source: Data[Source]): Data.Tuple = {
    val targetCoordinates: IndexedSeq[Data.Any] = coordinatePickers.map(_.pick(source))
    val targetOrigin = source.origin.computed

    Data.Tuple(targetCoordinates)(targetOrigin, targetSchema)
  }
}