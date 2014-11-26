package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.data.Data

case class SimpleComputedSeqData[+ElementShape <: Shape.Any](
  val data: Data.Seq[ElementShape],
  val origin: Origin,
  val elements: scala.Seq[OriginatedData[ElementShape]])
    extends OriginatedSeqData[ElementShape]