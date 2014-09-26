package com.github.comco.scrappy.picker

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

/**
 * Base class for pickers on tuples.
 */
abstract class BaseTuplePicker extends Picker {
  def sourceType: TupleType

  def pickData(source: Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[TupleData])
  } ensuring (_.datatype == targetType)

  def doPickData(source: TupleData): Data

  def pickOriginatedData(source: OriginatedData) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedTupleData])
  } ensuring (_.datatype == targetType)

  def doPickOriginatedData(source: OriginatedTupleData): OriginatedData
}