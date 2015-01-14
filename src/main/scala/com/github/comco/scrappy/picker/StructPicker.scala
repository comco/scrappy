package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class StructPicker[-Source <: Shape.Any](
  val targetSchema: Schema.Struct,
  val featurePickers: Map[String, Picker[Source, Shape.Any]])(
    implicit dataFactory: Data.Factory)
    extends BasePicker[Source, Shape.Struct] {

  override def sourceSchema = featurePickers.head._2.sourceSchema

  override def doPick(source: Data[Source]): Data.Struct = {
    val targetFeatures: Map[String, Data.Any] = featurePickers.map {
      case (name, picker) => (name, picker.pick(source))
    }

    val targetOrigin = source.origin.computed

    dataFactory.struct(targetSchema, targetOrigin, targetFeatures)
  }
}