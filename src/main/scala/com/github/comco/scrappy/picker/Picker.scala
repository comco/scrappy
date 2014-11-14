package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.SeqType

/**
 * Represents data transformations. A picker instance works both on bare data
 * and on originated data.
 */
abstract class Picker {
  def sourceType: Type
  def targetType: Type

  def pickData(source: Data): Data
  def pickOriginatedData(source: OriginatedData): OriginatedData

  def compatibleWith(that: Picker) = this.sourceType compatibleWith that.sourceType
}

object Picker {
  type ReturningSeq = Picker {
    def targetType: SeqType
    def pickData(source: Data): SeqData
    def pickOriginatedData(source: OriginatedData): OriginatedSeqData
  }
}