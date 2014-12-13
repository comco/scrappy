package com.github.comco.scrappy.data.simple

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData

case class SimpleSeqData(val datatype: SeqType, val elements: Seq[Data])
  extends SeqData