package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

/**
 * Composes pickers of comparable types together.
 */
case class AndThenPicker[-Source <: Shape.Any, Middle <: Shape.Any, +Target <: Shape.Any](val first: Picker[Source, Middle], val next: Picker[Middle, Target])
    extends Picker[Source, Target] {
  require(first.targetType.isSubtypeOf(next.sourceType))

  def sourceType = first.sourceType
  def targetType = next.targetType

  def pickData(source: Data[Source]) = next.pickData(first.pickData(source))

  def pickOriginatedData(source: OriginatedData[Source]) =
    next.pickOriginatedData(first.pickOriginatedData(source))
}