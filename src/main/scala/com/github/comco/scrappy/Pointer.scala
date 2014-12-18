package com.github.comco.scrappy

import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.AndThenPicker
import com.github.comco.scrappy.picker.MapPicker
import com.github.comco.scrappy.picker.FlatMapPicker
import com.github.comco.scrappy.picker.FlatMapPicker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.AndThenPicker

/**
 * A pointer is a sequence of steps, identifying the position of a target
 * from a source. Pointers can grow by appending new steps to their target side.
 */
sealed abstract class Pointer[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceType: Type[Source]
  def targetType: Type[Target]

  def picker: Picker[Source, Target]

  def mkPicker[NewTarget <: Shape.Any](tailPicker: Picker[Target, NewTarget]): Picker[Source, NewTarget]

  /**
   * Appends a step (at the end) to this pointer.
   */
  def append(step: Step): Pointer = StepPointer(this, step)

  /**
   * Prepends a step (at the beginning) to this pointer.
   */
  def prepend(step: Step): Pointer = step.mkPointer().concat(this)

  /**
   * Concatenates a pointer to this pointer.
   */
  def concat(pointer: Pointer): Pointer = {
    require(targetType == pointer.sourceType,
      s"Pointer $pointer has incompatible type for concatenation with $this.")
    pointer match {
      case SelfPointer(_) => this
      case StepPointer(init, step) => concat(init).append(step)
    }
  }

  /**
   * Constructs the longest common ancestor between this pointer and that pointer.
   */
  def longestCommonAncestor(that: Pointer): Pointer = {
    require(this.sourceType == that.sourceType,
      s"Pointer $that has incompatible source type with $this.")
    val results = steps.zip(that.steps).takeWhile({
      case (step1, step2) => step1.intersect(step2).isDefined
    }).map {
      case (step1, step2) => step1.intersect(step2).get
    }

    results.foldLeft[Pointer](SelfPointer(sourceType))(_.append(_))
  }

  private def stepsImpl(result: List[Step]): List[Step] = this match {
    case SelfPointer(_) => result
    case StepPointer(init, step) => init.stepsImpl(step :: result)
  }

  /**
   * Returns a list of the steps of this pointer.
   */
  def steps: List[Step] = stepsImpl(List.empty)
}

/**
 * A base case of a pointer which points to its source.
 */
case class SelfPointer(val sourceType: Type) extends Pointer {
  def targetType = sourceType

  lazy val picker = SelfPicker(sourceType)

  def mkPicker(tailPicker: Picker): Picker = {
    require(tailPicker.sourceType == sourceType)
    tailPicker
  }
}

/**
 * A general case of a pointer which has an init part and a last step.
 */
case class StepPointer(init: Pointer, step: Step) extends Pointer {
  require(init.targetType == step.sourceType,
    s"Last step $step has incompatible type with init pointer $init")

  lazy val sourceType = init.sourceType
  lazy val targetType = step.targetType

  lazy val picker: Picker = mkPicker(SelfPicker(targetType))

  def mkPicker(tailPicker: Picker): Picker = {
    if (tailPicker.isInstanceOf[SelfPicker]) {
      init.mkPicker(step.picker)
    } else step match {
      case IntoStep(_) => init.mkPicker(MapPicker(tailPicker))
      case _ => init.mkPicker(AndThenPicker(step.picker, tailPicker))
    }
  }
}