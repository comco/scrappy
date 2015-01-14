package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicSomeData[+Value <: Shape.Concrete](
  val schema: Schema.Optional[Value],
  val origin: Origin.Optional[Value],
  val value: Data[Value])
    extends Data.RichSome[Value]