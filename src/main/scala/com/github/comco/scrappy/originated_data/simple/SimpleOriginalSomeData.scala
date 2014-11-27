package com.github.comco.scrappy.originated_data.simple

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSomeData
import com.github.comco.scrappy.pointer.SomeStep
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.Shape

case class SimpleOriginalSomeData[+ValueShape <: Shape.Concrete: TypeTag](val data: SomeData[ValueShape], val origin: Origin)
    extends OriginatedSomeData[ValueShape] {

  lazy val value: OriginatedData[ValueShape] = {
    OriginatedData.from(data.value, origin.append(SomeStep(datatype)))
  }
}