package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data._
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.simple.SimpleOriginatedNoneData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer

sealed abstract class OriginatedData {
  require(data.datatype == origin.targetType,
    s"Datatype of data: $data and targetType of origin: $origin does not match.")

  /**
   * The type of this originated data.
   */
  def datatype: Type

  def data: Data
  def origin: Origin
}

object OriginatedData {
  private[originated_data] abstract class Base extends OriginatedData

  /**
   * Checks if data contains a value.
   * OptionData in case of none data doesn't contain a value.
   */
  def isFilled(data: OriginatedData): Boolean = data match {
    case data: OriginatedOptionData => data.isSome
    case _ => true
  }

  def from(data: Data, origin: Origin): OriginatedData = data match {
    case data: PrimitiveData[t] => OriginatedPrimitiveData[t](data, origin)
    case data: TupleData => OriginatedTupleData.original(data, origin)
    case data: StructData => OriginatedStructData.original(data, origin)
    case data: SeqData => OriginatedSeqData.original(data, origin)
    case data: SomeData => OriginatedSomeData.original(data, origin)
    case data: NoneData => SimpleOriginatedNoneData(origin)
    case _ => throw new IllegalStateException(
        s"Data: $data cannot be matched against known datatypes.")
  }

  def from(data: Data, source: OriginatedData): OriginatedData = {
    from(data, source.origin.computedWithTargetType(data.datatype))
  }

  def fromSelf(data: Data): OriginatedData = {
    from(data, OriginalOrigin(SelfPointer(data.datatype)))
  }
}