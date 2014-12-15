package com.github.comco.scrappy.data

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy.Data

case class DefaultPrimitiveData[+RawType: TypeTag](val value: RawType) extends Data.RichPrimitive[RawType]