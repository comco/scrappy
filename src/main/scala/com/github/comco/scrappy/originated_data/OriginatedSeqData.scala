package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData

abstract class OriginatedSeqData extends OriginatedData.Base {
  def datatype: SeqType = data.datatype
  def data: SeqData
}