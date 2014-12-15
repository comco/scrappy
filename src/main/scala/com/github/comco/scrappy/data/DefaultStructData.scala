package com.github.comco.scrappy.data

import com.github.comco.scrappy.Data
import com.github.comco.scrappy.Type

case class DefaultStructData(val datatype: Type.Struct, val features: Map[String, Data.Any]) extends Data.RichStruct {
  // TODO: Maybe validate the parameters
}