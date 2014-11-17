package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.Type

case class SimpleComputedSeqData(
  val data: SeqData,
  val origin: Origin,
  val elements: Seq[OriginatedData[Type[Any]]])
    extends OriginatedSeqData