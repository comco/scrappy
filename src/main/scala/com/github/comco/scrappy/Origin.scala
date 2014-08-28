package com.github.comco.scrappy

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
  def computed: Computed
  
  /**
   * Constructs a computed origin with a some target type from this origin.
   */
  def computedWithTargetType(targetType: Type): Computed
}

case class Original(val pointer: Pointer) extends Origin {
  
  def sourceType = pointer.sourceType
  def targetType = pointer.targetType

  def append(step: Step): Original = {
    require(step.sourceType == targetType,
      s"Cannot append step: $step to an origin: $this")
    
    Original(pointer.append(step))
  }

  def computed = Computed(sourceType, targetType, Set(pointer))
  
  def computedWithTargetType(targetType: Type) = 
    Computed(sourceType, targetType, Set(pointer))
}

case class Computed(
  val sourceType: Type,
  val targetType: Type,
  val pointers: Set[Pointer])
    extends Origin {
  
  def append(step: Step): Computed = {
    require(step.sourceType == targetType,
      s"Cannot append step: $step to an origin: $this")
    Computed(sourceType, step.targetType, pointers)
  }

  def computed = this
  def computedWithTargetType(targetType: Type) = Computed(sourceType, targetType, pointers)
}