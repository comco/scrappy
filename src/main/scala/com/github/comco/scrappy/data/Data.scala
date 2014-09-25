package com.github.comco.scrappy.data

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.OptionType

sealed abstract class Data {
  /**
   * The type of this data.
   */
  def datatype: Type
}

object Data {
  private[data] abstract class Base extends Data

  /**
   * Checks if data contains a value.
   * OptionData in case of none data doesn't contain a value.
   */
  def isFilled(data: Data): Boolean = data match {
    case data: OptionData => data.isSome
    case _ => true
  }

  def canAssign(datatype: Type, data: Data): Boolean = {
    data.datatype == datatype ||
      (datatype.isInstanceOf[OptionType] &&
        datatype.asInstanceOf[OptionType].someType == data.datatype)
  }

  def convert(datatype: Type, data: Data): Data = {
    assert(canAssign(datatype, data))
    if (data.datatype == datatype) {
      data
    } else {
      SomeData(datatype.asInstanceOf[OptionType], data)
    }
  }
}


