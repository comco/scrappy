package com.github.comco.scrappy.data.simple

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy._

case class SimplePrimitive[+RawType: TypeTag](val raw: RawType) extends Data.RichPrimitive[RawType] {
  def datatype = Type.Primitive[RawType]
}

case class SimpleSome[+Value <: Shape.Concrete: TypeTag](val value: Data[Value]) extends Data.RichSome[Value] {
  def datatype = Type.Some(value)
}

object SimpleData extends Data.Factory {
  def primitive[RawType: TypeTag](raw: RawType) = SimplePrimitive(raw)
}