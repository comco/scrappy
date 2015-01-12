package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicStructData[-Source <: Shape.Any](
  val features: Map[String, Data[Source, Shape.Any]],
  val origin: Origin[Source, Shape.Struct],
  val schema: Schema.Struct)
    extends Data.RichStruct[Source]