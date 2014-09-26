package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data._
import com.github.comco.scrappy.originated_data._
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.originated_data.OriginatedSomeData
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.data.StructData

/**
 * Represents data transformations. A picker instance works both on bare data
 * and on originated data.
 */
abstract class Picker {
  def sourceType: Type
  def targetType: Type

  def pickData(source: Data): Data
  def pickOriginatedData(source: OriginatedData): OriginatedData
}
