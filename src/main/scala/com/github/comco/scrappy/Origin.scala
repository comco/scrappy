package com.github.comco.scrappy

/**
 * Representation of the origin of a value - either it is original or computed.
 */
sealed abstract class Origin[-Source <: Shape.Any, +Target <: Shape.Any] {
  /**
   * The source type of this origin.
   */
  def sourceType: Type[Source]

  /**
   * The target type of this origin.
   */
  def targetType: Type[Target]

  /**
   * Constructs an origin by appending a step to this origin.
   */
  def append[NewTarget <: Shape.Any](step: Step[Target, NewTarget]): Origin[Source, NewTarget]

  /**
   * Constructs a computed origin from this origin.
   */
  def computed: ComputedOrigin[Source, Target]

  /**
   * Constructs a computed origin with a some target type from this origin.
   */
  def computedWithTargetType[NewTarget <: Shape.Any](targetType: Type[NewTarget]): ComputedOrigin[Source, NewTarget]

  def pointers: Set[Pointer.Any]
}

case class OriginalOrigin[-Source <: Shape.Any, +Target <: Shape.Any](val pointer: Pointer[Source, Target]) extends Origin[Source, Target] {
  def sourceType = pointer.sourceType
  def targetType = pointer.targetType

  def append[NewTarget <: Shape.Any](step: Step[Target, NewTarget]) = {
    require(step.sourceType == targetType,
      s"Cannot append step: $step to an origin: $this")

    OriginalOrigin(pointer.append(step))
  }

  def computed = ComputedOrigin(sourceType, targetType, Set(pointer))

  def computedWithTargetType[NewTarget <: Shape.Any](targetType: Type[NewTarget]) =
    ComputedOrigin(sourceType, targetType, Set(pointer))

  lazy val pointers: Set[Pointer[Source, Target]] = Set(pointer)
}

case class ComputedOrigin[-Source <: Shape.Any, +Target <: Shape.Any](
  val sourceType: Type[Source],
  val targetType: Type[Target],
  val pointers: Set[Pointer.Any])
    extends Origin[Source, Target] {

  def append[NewTarget <: Shape.Any](step: Step[Target, NewTarget]): ComputedOrigin[Source, NewTarget] = {
    require(targetType <:< step.sourceType, s"Target type $targetType of origin is incompatible with source type ${step.sourceType} of step.")
    computedWithTargetType(step.targetType)
  }

  def computed = this

  def computedWithTargetType[NewTarget <: Shape.Any](targetType: Type[NewTarget]) =
    ComputedOrigin(sourceType, targetType, pointers)
}