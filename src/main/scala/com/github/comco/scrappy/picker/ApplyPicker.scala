package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy._

case class ApplyPicker[RawSource: TypeTag, RawTarget: TypeTag](
  f: RawSource => RawTarget)(
    implicit schemaFactory: Schema.Factory, dataFactory: Data.Factory)
    extends BasePicker[Shape.Primitive[RawSource], Shape.Primitive[RawTarget]] {
  override def sourceSchema = Schema.Primitive[RawSource]
  override def targetSchema = Schema.Primitive[RawTarget]

  override def doPick(source: Data.Primitive[RawSource]): Data.Primitive[RawTarget] = {
    val targetOrigin = source.origin.computed
    Data.Primitive(f(source.raw), targetOrigin, targetSchema)
  }
}