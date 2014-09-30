package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalSomeData

abstract class OriginatedSomeData extends OriginatedOptionData.PackageSealed {
  def datatype = data.datatype
  def data: SomeData

  def value: OriginatedData
}

object OriginatedSomeData {
  def original(data: SomeData, origin: Origin): OriginatedSomeData = {
    SimpleOriginalSomeData(data, origin)
  }
}