package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicStructData(
  val features: Map[String, Data.Any],
  val origin: Origin.Struct,
  val schema: Schema.Struct)
    extends Data.RichStruct