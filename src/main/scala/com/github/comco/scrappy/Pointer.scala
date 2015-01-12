package com.github.comco.scrappy

import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.Coordinate11Picker
import com.github.comco.scrappy.picker.Coordinate21Picker
import com.github.comco.scrappy.picker.Coordinate22Picker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.ValuePicker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.AndThenPicker

sealed abstract class Pointer[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceSchema: Schema.Any

  def targetSchema: Schema[Target]

  def picker: Picker[Source, Target]
}

object Pointer {
  type Any = Pointer[Shape.Any, Nothing]

  case class Self[Shape <: Shape.Any](val sourceSchema: Schema[Shape])
      extends Pointer[Shape, Shape] {

    override def targetSchema = sourceSchema

    override def picker = SelfPicker(sourceSchema)
  }

  case class Cons[-Source <: Shape.Any, Middle <: Shape.Any, +Target <: Shape.Any](
    val init: Pointer[Source, Middle],
    val last: Step[Middle, Target])
      extends Pointer[Source, Target] {

    override def sourceSchema = init.sourceSchema

    override def targetSchema = last.targetSchema

    override def picker = AndThenPicker(init.picker, last.picker)
  }
}