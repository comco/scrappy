package com.github.comco.scrappy.originated_data

import scala.reflect.runtime.universe._
import com.github.comco.scrappy._

case class DefaultOriginatedPrimitiveData[+RawType: TypeTag](val data: Data.Primitive[RawType], val origin: Origin[Shape.Primitive[RawType]]) extends OriginatedData.RichPrimitive[RawType] {

}