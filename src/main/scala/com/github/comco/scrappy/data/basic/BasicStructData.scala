package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicStructData(
  val schema: Schema.Struct,
  val origin: Origin[Shape.Struct],
  val features: Map[String, Data[Shape.Any]])
    extends Data.RichStruct