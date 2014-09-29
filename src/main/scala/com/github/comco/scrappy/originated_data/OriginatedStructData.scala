package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.simple.SimpleComputedStructData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalStructData

abstract class OriginatedStructData extends OriginatedData.Base {
  def datatype: StructType = data.datatype
  def data: StructData

  def features: Map[String, OriginatedData]
}

object OriginatedStructData {
  def original(data: StructData, origin: Origin): OriginatedStructData =
    SimpleOriginalStructData(data, origin)

  def computed(data: StructData,
    origin: Origin,
    features: Map[String, OriginatedData]): OriginatedStructData =
    SimpleComputedStructData(data, origin, features)
}