package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class ValuePicker[Value <: Shape.Concrete](val sourceSchema: Schema.Optional[Value])
    extends BasePicker[Shape.Optional[Value], Value] {
  override def targetSchema = sourceSchema.valueSchema

  override def doPick(source: Data.Optional[Value]): Data[Value] = source match {
    case source: Data.RichSome[Value] => source.value
    case _ => throw new IllegalArgumentException(s"Optional data: $source does not contain a value.")
  }
}