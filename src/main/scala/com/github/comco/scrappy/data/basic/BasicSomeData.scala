package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicSomeData[+Value <: Shape.Concrete](
  val value: Data[Value],
  val origin: Origin.Optional[Value],
  val schema: Schema.Optional[Value])
    extends Data.RichSome[Value]