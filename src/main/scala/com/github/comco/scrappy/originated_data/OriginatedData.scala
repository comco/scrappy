package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.data.Data

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
  
  def from(data: Data, origin: Origin): OriginatedData = ???
  
  def from(data: Data, source: OriginatedData): OriginatedData = ???
  
  def fromSelf(data: Data): OriginatedData = ???
}