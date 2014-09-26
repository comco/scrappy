package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalSeqData
import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.originated_data.simple.SimpleComputedSeqData

abstract class OriginatedSeqData extends OriginatedData.Base {
  def datatype: SeqType = data.datatype
  def data: SeqData
  def elements: Seq[OriginatedData]
}

object OriginatedSeqData {
  def original(data: SeqData, origin: Origin): OriginatedSeqData =
    SimpleOriginalSeqData(data, origin)

  def computed(data: SeqData,
    origin: Origin,
    elements: Seq[OriginatedData]): OriginatedSeqData =
    SimpleComputedSeqData(data, origin, elements)

  def apply(data: SeqData, origin: Origin): OriginatedSeqData =
    original(data, origin)

  def apply(data: SeqData,
    origin: Origin,
    elements: Seq[OriginatedData]): OriginatedSeqData =
    computed(data, origin, elements)
}