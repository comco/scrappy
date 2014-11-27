package com.github.comco.scrappy.pointer

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.picker.Picker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.AndThenPicker
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.picker.MapPicker
import com.github.comco.scrappy.picker.FlatMapPicker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.AndThenPicker
import com.github.comco.scrappy.Shape

/**
 * A pointer is a sequence of steps, identifying the position of a target
 * from a source. Pointers can grow by appending new steps to their target side.
 */
sealed abstract class Pointer[-SourceShape <: Shape.Any: TypeTag, +TargetShape <: Shape.Any: TypeTag] {
  def sourceType: Type.Any
  def targetType: Type[TargetShape]

  def picker: Picker[SourceShape, TargetShape]

  /**
   * Appends a step after this pointer.
   */
  def append[NewTargetShape <: Shape.Any: TypeTag](step: Step[TargetShape, NewTargetShape]) = StepPointer(this, step)

  /**
   * Prepends a step (at the beginning) to this pointer.
   */
  def prepend[NewSourceShape <: Shape.Any: TypeTag](step: Step[NewSourceShape, SourceShape]): Pointer[NewSourceShape, TargetShape] =
    step.pointer.concat(this)

  /**
   * Concatenates a pointer to this pointer.
   */
  def concat[NewTargetShape <: Shape.Any: TypeTag](pointer: Pointer[TargetShape, NewTargetShape]): Pointer[SourceShape, NewTargetShape] = {
    require(targetType == pointer.sourceType,
      s"Pointer $pointer has incompatible type for concatenation with $this.")
    pointer match {
      case SelfPointer(_) => this.asInstanceOf[Pointer[SourceShape, NewTargetShape]]
      case StepPointer(init, step) => concat(init).append(step).asInstanceOf[Pointer[SourceShape, NewTargetShape]]
    }
  }
}

/**
 * A base case of a pointer which points to its source.
 */
case class SelfPointer[SelfShape <: Shape.Any: TypeTag](val sourceType: Type[SelfShape]) extends Pointer[SelfShape, SelfShape] {
  def targetType = sourceType

  lazy val picker = SelfPicker(sourceType)

  def mkPicker(tailPicker: Picker.Any): Picker.Any = {
    require(tailPicker.sourceType == sourceType)
    tailPicker
  }
}

/**
 * A general case of a pointer which has an init part and a last step.
 */
case class StepPointer[SourceShape <: Shape.Any: TypeTag, MiddleShape <: Shape.Any: TypeTag, TargetShape <: Shape.Any: TypeTag](
    init: Pointer[SourceShape, MiddleShape],
    step: Step[MiddleShape, TargetShape]) extends Pointer[SourceShape, TargetShape] {
  require(init.targetType == step.sourceType,
    s"Last step $step has incompatible type with init pointer $init")

  lazy val sourceType = init.sourceType
  lazy val targetType = step.targetType

  lazy val picker = AndThenPicker(init.picker, step.picker)
}