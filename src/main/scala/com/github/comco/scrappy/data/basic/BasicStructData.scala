package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicStructData(
  val features: Map[String, Data[Shape.Any]],
  val origin: Origin[Shape.Struct],
  val schema: Schema.Struct)
    extends Data.RichStruct