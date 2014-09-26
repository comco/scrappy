package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Base class for pickers on seq-s.
 */
abstract class BaseSeqPicker extends Picker {
  def sourceType: SeqType

  def pickData(source: Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[SeqData])
  } ensuring (_.datatype == targetType)

  def doPickData(source: SeqData): Data

  def pickOriginatedData(source: OriginatedData) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedSeqData])
  } ensuring (_.datatype == targetType)

  def doPickOriginatedData(source: OriginatedSeqData): OriginatedData
}