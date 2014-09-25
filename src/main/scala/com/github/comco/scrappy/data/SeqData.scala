package com.github.comco.scrappy.data

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.simple.SimpleSeqData

abstract class SeqData extends Data.Base {
  def datatype: SeqType

  /**
   * The elements of this seq data.
   */
  def elements: Seq[Data]

  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(index: Int): Boolean = {
    0 <= index && index < length && Data.isFilled(elements(index))
  }

  /**
   * Retrieves the element of this seq data at some index.
   */
  def element(index: Int): Data = {
    require(0 <= index && index < length,
      s"Index: $index is out of bounds for SeqData with length: $length.")

    elements(index)
  }

  /**
   * The length of this seq data.
   */
  def length: Int = elements.length
}

abstract class BaseSeqDataCompanionObject {
  def apply(datatype: SeqType, rawElements: Seq[Data]): SeqData = {
    require(rawElements.forall(Data.canAssign(datatype.elementType, _)),
      "An element has invalid datatype")
    val elements = rawElements.map(Data.convert(datatype.elementType, _))
    doApply(datatype, elements)
  }

  def doApply(datatype: SeqType, elements: Seq[Data]): SeqData
}

object SeqData extends BaseSeqDataCompanionObject {
  def doApply(datatype: SeqType, elements: Seq[Data]): SeqData =
    SimpleSeqData(datatype, elements)
  
  def apply(datatype: SeqType)(elements: Data*)(implicit dummy: DummyImplicit): SeqData = {
    SeqData(datatype, elements)
  }

  def apply(firstElement: Data, nextElements: Data*): SeqData = {
    SeqData(SeqType(firstElement.datatype), firstElement +: nextElements.toSeq)
  }
}