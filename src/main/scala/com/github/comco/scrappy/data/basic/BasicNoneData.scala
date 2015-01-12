package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicNoneData[-Source <: Shape.Any](
  val origin: Origin[Source, Shape.None],
  val schema: Schema.None)
    extends Data.RichNone[Source]