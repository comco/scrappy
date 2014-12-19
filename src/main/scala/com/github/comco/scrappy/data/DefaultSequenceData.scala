package com.github.comco.scrappy.data

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy._

case class DefaultSequenceData[+Element <: Shape.Any: TypeTag](val elements: Seq[Data[Element]]) extends Data.RichSequence[Element]