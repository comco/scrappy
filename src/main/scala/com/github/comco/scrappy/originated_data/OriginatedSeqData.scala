package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalSeqData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.simple.SimpleComputedSeqData

abstract class OriginatedSeqData extends OriginatedData.PackageSealed {
  def datatype: SeqType = data.datatype
  def data: SeqData

  /**
   * The elements of this originated seq data.
   */
  def elements: Seq[OriginatedData]

  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(index: Int): Boolean = {
    0 <= index && index < length && OriginatedData.isFilled(elements(index))
  }
  
  /**
   * Retrieves the element of this seq data at some index.
   */
  def element(index: Int): OriginatedData = {
    require(0 <= index && index < length,
      s"Index: $index is out of bounds for SeqData with length: $length.")

    elements(index)
  }
  
  /**
   * The length of this seq data.
   */
  def length: Int = elements.length
}

object OriginatedSeqData {
  def original(data: SeqData, origin: Origin): OriginatedSeqData =
    SimpleOriginalSeqData(data, origin)

  def computed(data: SeqData,
    origin: Origin,
    elements: Seq[OriginatedData]): OriginatedSeqData =
    SimpleComputedSeqData(data, origin, elements)

  def apply(data: SeqData, origin: Origin): OriginatedSeqData =
    original(data, origin)

  def apply(data: SeqData,
    origin: Origin,
    elements: Seq[OriginatedData]): OriginatedSeqData =
    computed(data, origin, elements)
}