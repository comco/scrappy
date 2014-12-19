package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy._

case class SomePicker[+Value <: Shape.Concrete: TypeTag](val sourceType: Type.Some[Value])
    extends Picker[Shape.Optional[Value], Value] {

  def targetType = sourceType.valueType

  def pickData(source: Data.Optional[Value]) = source match {
    case source: Data.RichSome[Value] => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick Data.None.")
  }

  def pickOriginatedData(source: OriginatedData.Optional[Value]) = source match {
    case source: OriginatedData.RichSome[Value] => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick OriginatedData.None.")
  }
}