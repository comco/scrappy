package com.github.comco.scrappy

sealed abstract class Pointer {
  def sourceType: Type
  def targetType: Type
  
  def picker: Picker
  
  def append(step: Step): Pointer = StepPointer(this, step)
}

case class SelfPointer(val sourceType: Type) extends Pointer {
  def targetType = sourceType
  
  def picker = SelfPicker(sourceType)
}

case class StepPointer(init: Pointer, step: Step) extends Pointer {
  require(init.targetType == step.sourceType,
    s"Last step $step has incompatible type with init pointer $init")

  def sourceType = init.sourceType
  def targetType = step.targetType
  
  def picker = AndThenPicker(init.picker, step.picker)
}