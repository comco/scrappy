package com.github.comco.scrappy.picker

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

/**
 * Base class for pickers on tuples.
 */
abstract class BaseTuplePicker extends BasePicker {
  def sourceType: TupleType

  def doPickData(source: Data) = {
    doPickData(source.asInstanceOf[TupleData])
  }

  def doPickData(source: TupleData): Data

  def doPickOriginatedData(source: OriginatedData) = {
    doPickOriginatedData(source.asInstanceOf[OriginatedTupleData])
  }

  def doPickOriginatedData(source: OriginatedTupleData): OriginatedData
}