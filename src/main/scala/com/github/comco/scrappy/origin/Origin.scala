package com.github.comco.scrappy.origin

import com.github.comco.scrappy.pointer.Pointer
import com.github.comco.scrappy.pointer.Step
import com.github.comco.scrappy.Type

/**
 * Representation of the origin of a value - either it is original or computed.
 */
sealed abstract class Origin {

  /**
   * The source type of this origin.
   */
  def sourceType: Type

  /**
   * The target type of this origin.
   */
  def targetType: Type

  /**
   * Constructs an origin by appending a step to this origin.
   */
  def append(step: Step): Origin

  /**
   * Constructs a computed origin from this origin.
   */
  def computed: ComputedOrigin

  /**
   * Constructs a computed origin with a some target type from this origin.
   */
  def computedWithTargetType(targetType: Type): ComputedOrigin
}

case class OriginalOrigin(val pointer: Pointer) extends Origin {

  def sourceType = pointer.sourceType
  def targetType = pointer.targetType

  def append(step: Step): OriginalOrigin = {
    require(step.sourceType == targetType,
      s"Cannot append step: $step to an origin: $this")

    OriginalOrigin(pointer.append(step))
  }

  def computed = ComputedOrigin(sourceType, targetType, Set(pointer))

  def computedWithTargetType(targetType: Type) =
    ComputedOrigin(sourceType, targetType, Set(pointer))
}

case class ComputedOrigin(
  val sourceType: Type,
  val targetType: Type,
  val pointers: Set[Pointer])
    extends Origin {

  def append(step: Step): ComputedOrigin = {
    require(step.sourceType == targetType,
      s"Cannot append step: $step to an origin: $this")
    ComputedOrigin(sourceType, step.targetType, pointers)
  }

  def computed = this
  
  def computedWithTargetType(targetType: Type) =
    ComputedOrigin(sourceType, targetType, pointers)
}