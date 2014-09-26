package com.github.comco.scrappy.data

import scala.language.implicitConversions

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.simple.SimpleSomeData

abstract class SomeData extends OptionData.Base {
  def isSome = true

  /**
   * The value of this option data.
   */
  def value: Data
}

abstract class BaseSomeDataCompanionObject {
  def apply(datatype: OptionType, value: Data): SomeData = {
    require(value.datatype == datatype.someType,
      s"SomeData: $this cannot be created with value of type: ${value.datatype}")
    doApply(datatype, value)
  }

  def doApply(datatype: OptionType, value: Data): SomeData

  implicit def apply[T](value: T)(
    implicit datatype: PrimitiveType[T]): SomeData = {
    apply(OptionType(datatype), PrimitiveData(value))
  }
}

object SomeData extends BaseSomeDataCompanionObject {
  def doApply(datatype: OptionType, value: Data): SomeData =
    SimpleSomeData(datatype, value)
}