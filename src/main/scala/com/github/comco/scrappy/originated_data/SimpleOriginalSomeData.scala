package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSomeData
import com.github.comco.scrappy.pointer.SomeStep

case class SimpleOriginalSomeData(val data: SomeData, val origin: Origin)
    extends OriginatedSomeData {

  lazy val value: OriginatedData = {
    OriginatedData.from(data.value, origin.append(SomeStep(datatype)))
  }
}