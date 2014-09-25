package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.ElementStep
import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

case class SimpleOriginalSeqData(val data: SeqData, val origin: Origin)
    extends OriginatedSeqData {

  lazy val elements: Seq[OriginatedData] = data.elements.zipWithIndex.map {
    case (elem, index) =>
      val elemOrigin = origin.append(ElementStep(datatype, index))
      OriginatedData.from(elem, elemOrigin)
  }
}