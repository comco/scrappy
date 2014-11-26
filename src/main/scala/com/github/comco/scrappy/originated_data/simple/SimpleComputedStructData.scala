package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.Type

case class SimpleComputedStructData(
  val data: StructData,
  val origin: Origin,
  val features: Map[String, OriginatedData.Any])
    extends OriginatedStructData