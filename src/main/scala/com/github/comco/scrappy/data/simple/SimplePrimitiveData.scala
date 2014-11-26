package com.github.comco.scrappy.data.simple

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.PrimitiveData

case class SimplePrimitiveData[+RawType: TypeTag](val value: RawType)(
  implicit val datatype: PrimitiveType[RawType])
    extends PrimitiveData[RawType]