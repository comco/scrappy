package com.github.comco.scrappy.data.simple

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.Type

case class SimpleSeqData(val datatype: SeqType, val elements: Seq[Data[Type[Any]]])
  extends SeqData