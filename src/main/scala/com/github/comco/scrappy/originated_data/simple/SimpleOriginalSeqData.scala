package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.Shape

case class SimpleOriginalSeqData[+ElementShape <: Shape.Any](
  val data: Data.Seq[ElementShape],
  val origin: Origin)
    extends OriginatedSeqData {

  lazy val elements: scala.Seq[OriginatedData[ElementShape]] = data.elements.zipWithIndex.map {
    case (elem, index) =>
      val elemOrigin = origin.append(ElementStep(datatype, index))
      OriginatedData.from(elem, elemOrigin)
  }
}