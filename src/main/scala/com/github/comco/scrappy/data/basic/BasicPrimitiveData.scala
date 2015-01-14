package com.github.comco.scrappy.data.basic

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy._

case class BasicPrimitiveData[Raw: TypeTag](
  val schema: Schema.Primitive[Raw],
  val origin: Origin.Primitive[Raw],
  val raw: Raw)
    extends Data.RichPrimitive[Raw]