package com.github.comco.scrappy.data.simple

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.Type

case class SimpleStructData(
  val datatype: StructType,
  val features: Map[String, Data.Any])
    extends StructData