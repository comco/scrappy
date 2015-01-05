package com.github.comco.scrappy

import com.github.comco.scrappy.picker._

/**
 * A pointer is a sequence of steps, identifying the position of a target
 * from a source. Pointers can grow by appending new steps to their target side.
 */
sealed abstract class Pointer[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceType: Type[Source]
  def targetType: Type[Target]

  def picker: Picker[Source, Target]

  /**
   * Appends a step (at the end) to this pointer.
   */
  def append[NewTarget <: Shape.Any](step: Step[Target, NewTarget]): Pointer[Source, NewTarget] =
    StepPointer(this.asInstanceOf[Pointer[Source, Shape.Nil]], step.asInstanceOf[Step[Shape.Any, NewTarget]])
}

object Pointer {
  type Any = Pointer[Shape.Nil, Shape.Any]
}

/**
 * A base case of a pointer which points to its source.
 */
case class SelfPointer[Self <: Shape.Any](val sourceType: Type[Self]) extends Pointer[Self, Self] {
  def targetType = sourceType

  lazy val picker = SelfPicker(sourceType)
}

/**
 * A general case of a pointer which has an init part and a last step.
 */
case class StepPointer[-Source <: Shape.Any, +Target <: Shape.Any](init: Pointer[Source, Shape.Nil], step: Step[Shape.Any, Target]) extends Pointer[Source, Target] {
  require(init.targetType == step.sourceType,
    s"Last step $step has incompatible type with init pointer $init")

  lazy val sourceType = init.sourceType
  lazy val targetType = step.targetType

  lazy val picker: Picker[Source, Target] = AndThenPicker(init.picker, step.picker)
}