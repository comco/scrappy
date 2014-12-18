package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedStructData

case class SimpleComputedStructData(
  val data: StructData,
  val origin: Origin,
  val features: Map[String, OriginatedData])
    extends OriginatedStructData