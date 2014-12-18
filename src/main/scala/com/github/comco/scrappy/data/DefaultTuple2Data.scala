package com.github.comco.scrappy.data

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy._

case class DefaultTuple2Data[+Coordinate1 <: Shape.Any: TypeTag, +Coordinate2 <: Shape.Any: TypeTag](
  val coordinate1: Data[Coordinate1],
  val coordinate2: Data[Coordinate2]) extends Data.RichTuple2[Coordinate1, Coordinate2]