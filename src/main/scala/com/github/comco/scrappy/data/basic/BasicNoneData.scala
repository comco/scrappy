package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicNoneData(
  val origin: Origin.None,
  val schema: Schema.None)
    extends Data.RichNone