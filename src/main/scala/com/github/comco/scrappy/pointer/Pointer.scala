package com.github.comco.scrappy.pointer

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.picker.Picker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.AndThenPicker

/**
 * A pointer is a sequence of steps, identifying the position of a target
 * from a source. Pointers can grow by appending new steps to their target side.
 */
sealed abstract class Pointer {
  def sourceType: Type
  def targetType: Type

  def picker: Picker

  def append(step: Step): Pointer = StepPointer(this, step)
}

/**
 * A base case of a pointer which points to its source.
 */
case class SelfPointer(val sourceType: Type) extends Pointer {
  def targetType = sourceType

  def picker = SelfPicker(sourceType)
}

/**
 * A general case of a pointer which has an init part and a last step.
 */
case class StepPointer(init: Pointer, step: Step) extends Pointer {
  require(init.targetType == step.sourceType,
    s"Last step $step has incompatible type with init pointer $init")

  def sourceType = init.sourceType
  def targetType = step.targetType

  def picker = AndThenPicker(init.picker, step.picker)
}