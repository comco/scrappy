package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Base class for pickers on seq-s.
 */
abstract class BaseSeqPicker extends BasePicker {
  def sourceType: SeqType

  def doPickData(source: Data) = {
    doPickData(source.asInstanceOf[SeqData])
  }

  def doPickData(source: SeqData): Data

  def doPickOriginatedData(source: OriginatedData) = {
    doPickOriginatedData(source.asInstanceOf[OriginatedSeqData])
  }

  def doPickOriginatedData(source: OriginatedSeqData): OriginatedData
}