package com.github.comco.scrappy.data

import scala.reflect.runtime.universe._

import com.github.comco.scrappy._

case class DefaultSomeData[+Value <: Shape.Concrete: TypeTag](val value: Data[Value]) extends Data.RichSome[Value]