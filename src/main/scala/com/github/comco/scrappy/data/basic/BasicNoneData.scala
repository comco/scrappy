package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicNoneData(
  val origin: Origin.None)
    extends Data.RichNone {
  override def schema = Schema.None
}