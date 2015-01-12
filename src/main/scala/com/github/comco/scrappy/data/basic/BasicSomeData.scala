package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicSomeData[-Source <: Shape.Any, +Value <: Shape.Concrete](
  val value: Data[Source, Value],
  val origin: Origin[Source, Shape.Some[Value]],
  val schema: Schema.Some[Value])
    extends Data.RichSome[Source, Value]