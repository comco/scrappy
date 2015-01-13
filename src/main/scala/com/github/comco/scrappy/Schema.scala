package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy.utils.StateEquality

sealed trait Schema[+Shape <: Shape.Any] {
  def satisfies(superSchema: Schema.Any): Boolean = {
    // TODO
    true
  }
}

object Schema extends Domain {
  type Abstract[+Shape <: Shape.Any] = Schema[Shape]

  type RichAny = Any

  abstract class RichPrimitive[Raw] extends Primitive[Raw] with StateEquality[RichPrimitive[Raw]] {
    def typeTag: TypeTag[Raw]

    protected override def state = (typeTag)
  }

  abstract class RichStruct extends Struct with StateEquality[RichStruct] {
    def name: String

    def featureSchemas: Map[String, Schema.Any]

    def hasFeatureNamed(featureName: String): Boolean = featureSchemas.contains(featureName)

    protected override def state = (name, featureSchemas)
  }

  abstract class RichTuple extends Tuple with StateEquality[RichTuple] {
    def coordinateSchemas: IndexedSeq[Schema.Any]

    def arity = coordinateSchemas.length

    def hasCoordinateAtPosition(position: Int): Boolean = (0 <= position && position < arity)

    protected override def state = (coordinateSchemas)
  }

  abstract class RichTuple1[+Coordinate1 <: Shape.Any] extends RichTuple with Tuple1[Coordinate1] {
    def coordinate1Schema: Schema[Coordinate1]

    override def coordinateSchemas = IndexedSeq(coordinate1Schema)
  }

  abstract class RichTuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] extends RichTuple with Tuple2[Coordinate1, Coordinate2] {
    def coordinate1Schema: Schema[Coordinate1]

    def coordinate2Schema: Schema[Coordinate2]

    override def coordinateSchemas = IndexedSeq(coordinate1Schema, coordinate2Schema)
  }

  abstract class RichSequence[+Element <: Shape.Any] extends Sequence[Element] with StateEquality[RichSequence[_]] {
    def elementSchema: Schema[Element]

    protected override def state = (elementSchema)
  }

  sealed abstract class RichOptional[+Value <: Shape.Concrete] extends Optional[Value] with StateEquality[RichOptional[_]] {
    def hasValue: Boolean

    def valueSchema: Schema[Value]

    protected override def state = (valueSchema)
  }

  abstract class RichSome[+Value <: Shape.Concrete] extends RichOptional[Value] with Some[Value] {
    override def hasValue = true
  }

  abstract class RichNone extends RichOptional[Nothing] with None {
    override def hasValue = false
  }

  abstract class RichDynamic extends Dynamic

  trait Factory {
    def primitive[Raw: TypeTag]: RichPrimitive[Raw]

    def struct(name: String, featureSchemas: Map[String, Schema.Any]): RichStruct

    def tuple(coordinateSchemas: IndexedSeq[Schema.Any]): RichTuple

    def tuple[Coordinate1 <: Shape.Any](coordinate1Schema: Schema[Coordinate1]): RichTuple1[Coordinate1]

    def tuple[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](coordinate1Schema: Schema[Coordinate1], coordinate2Schema: Schema[Coordinate2]): RichTuple2[Coordinate1, Coordinate2]

    def sequence[Element <: Shape.Any](elementSchema: Schema[Element]): RichSequence[Element]

    def optional[Value <: Shape.Concrete](valueSchema: Schema[Value]): RichSome[Value]

    def none: RichNone

    def any: RichAny = Any
  }

  object Primitive {
    def apply[Raw: TypeTag](implicit factory: Factory) = factory.primitive
  }

  object Tuple {
    def apply(coordinateSchemas: IndexedSeq[Schema.Any])(implicit factory: Factory) = factory.tuple(coordinateSchemas)

    def apply[Coordinate1 <: Shape.Any](coordinate1Schema: Schema[Coordinate1])(implicit factory: Factory) = factory.tuple(coordinate1Schema)

    def apply[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](coordinate1Schema: Schema[Coordinate1], coordinate2Schema: Schema[Coordinate2])(implicit factory: Factory) = factory.tuple(coordinate1Schema, coordinate2Schema)
  }

  object Sequence {
    def apply[Element <: Shape.Any](elementSchema: Schema[Element])(implicit factory: Factory) = factory.sequence(elementSchema)
  }

  object Optional {
    def apply[Value <: Shape.Concrete](valueSchema: Schema[Value])(implicit factory: Factory) = factory.optional(valueSchema)
  }

  object Any extends RichAny
}