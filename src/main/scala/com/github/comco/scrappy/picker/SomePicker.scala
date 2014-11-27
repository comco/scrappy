package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.originated_data.OriginatedSomeData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Shape

case class SomePicker[+ValueShape <: Shape.Concrete: TypeTag](val sourceType: Type.Optional[ValueShape])
    extends BasePicker[Shape.Optional[ValueShape], ValueShape] {

  def targetType = sourceType.valueType

  def doPickData(source: Data.Optional[ValueShape]) = source match {
    case source: SomeData[ValueShape] => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }

  def doPickOriginatedData(source: OriginatedData.Optional[ValueShape]) = source match {
    case source: OriginatedSomeData[ValueShape] => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }
}