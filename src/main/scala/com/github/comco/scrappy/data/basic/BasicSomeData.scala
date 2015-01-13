package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicSomeData[+Value <: Shape.Concrete](
  val value: Data[Value],
  val origin: Origin.Some[Value],
  val schema: Schema.Some[Value])
    extends Data.RichSome[Value]