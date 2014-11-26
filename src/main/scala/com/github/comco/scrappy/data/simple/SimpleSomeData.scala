package com.github.comco.scrappy.data.simple

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy.OptionalType
import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SomeData

case class SimpleSomeData[+ValueShape <: Shape.Concrete: TypeTag](
  val value: Data[ValueShape])
    extends SomeData[ValueShape] {
  lazy val datatype = OptionalType(value.datatype)
}