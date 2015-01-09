package com.github.comco.scrappy

import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.Coordinate11Picker
import com.github.comco.scrappy.picker.Coordinate21Picker
import com.github.comco.scrappy.picker.Coordinate22Picker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.ValuePicker

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

  case class Feature(val sourceSchema: Schema.Struct, val featureName: String)
      extends Pointer[Shape.Struct, Shape.Any] {

    override def targetSchema = sourceSchema.featureSchemas(featureName)

    override def picker = FeaturePicker(sourceSchema, featureName)
  }

  case class Coordinate(val sourceSchema: Schema.Tuple, val position: Int)
      extends Pointer[Shape.Tuple, Shape.Any] {

    override def targetSchema = sourceSchema.coordinateSchemas(position)

    override def picker = CoordinatePicker(sourceSchema, position)
  }

  case class Coordinate11[Coordinate1 <: Shape.Any](val sourceSchema: Schema.Tuple1[Coordinate1])
      extends Pointer[Shape.Tuple1[Coordinate1], Coordinate1] {

    override def targetSchema = sourceSchema.coordinate1Schema

    override def picker = Coordinate11Picker(sourceSchema)
  }

  case class Coordinate21[Coordinate1 <: Shape.Any](val sourceSchema: Schema.Tuple2[Coordinate1, Nothing])
      extends Pointer[Shape.Tuple2[Coordinate1, Nothing], Coordinate1] {

    override def targetSchema = sourceSchema.coordinate1Schema

    override def picker = Coordinate21Picker(sourceSchema)
  }

  case class Coordinate22[Coordinate2 <: Shape.Any](val sourceSchema: Schema.Tuple2[Nothing, Coordinate2])
      extends Pointer[Shape.Tuple2[Nothing, Coordinate2], Coordinate2] {

    override def targetSchema = sourceSchema.coordinate2Schema

    override def picker = Coordinate22Picker(sourceSchema)
  }

  case class Element[E <: Shape.Any](val sourceSchema: Schema.Sequence[E], val index: Int)
      extends Pointer[Shape.Sequence[E], E] {

    override def targetSchema = sourceSchema.elementSchema

    override def picker = ElementPicker(sourceSchema, index)
  }

  case class Value[V <: Shape.Concrete](val sourceSchema: Schema.Optional[V])
      extends Pointer[Shape.Optional[V], V] {

    override def targetSchema = sourceSchema.valueSchema

    override def picker = ValuePicker(sourceSchema)
  }
}