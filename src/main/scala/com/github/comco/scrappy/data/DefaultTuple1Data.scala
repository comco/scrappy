package com.github.comco.scrappy.data

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy._

case class DefaultTuple1Data[+Coordinate1 <: Shape.Any: TypeTag](val coordinate1: Data[Coordinate1]) extends Data.RichTuple1[Coordinate1]