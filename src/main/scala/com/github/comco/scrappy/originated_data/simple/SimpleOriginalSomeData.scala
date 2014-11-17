package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSomeData
import com.github.comco.scrappy.pointer.SomeStep
import com.github.comco.scrappy.Type

case class SimpleOriginalSomeData(val data: SomeData, val origin: Origin)
    extends OriginatedSomeData {

  lazy val value: OriginatedData[Type[Any]] = {
    OriginatedData.from(data.value, origin.append(SomeStep(datatype)))
  }
}