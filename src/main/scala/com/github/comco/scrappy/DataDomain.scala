package com.github.comco.scrappy

import scala.language.higherKinds

trait DataDomain extends Domain {
  trait Abstract[+Shape <: Shape.Any]

  trait Mixin[+Shape <: Shape.Any] {
    def datatype: Type[Shape]
  }

  trait PrimitiveMixin[+RawType] extends Mixin[Shape.Primitive[RawType]] {
    def raw: RawType
  }

  trait StructMixin extends Mixin[Shape.Struct] {
    def features: Map[String, Any]
  }

  trait TupleMixin extends Mixin[Shape.Tuple] {
    def coordinates: IndexedSeq[Any]
  }

  trait Tuple1Mixin[+Coordinate1 <: Shape.Any] extends TupleMixin {
    def coordinate1: Abstract[Coordinate1]
    override def coordinates = IndexedSeq(coordinate1)
  }

  trait Tuple2Mixin[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] extends TupleMixin {
    def coordinate1: Abstract[Coordinate1]
    def coordinate2: Abstract[Coordinate2]
    override def coordinates = IndexedSeq(coordinate1, coordinate2)
  }

  trait SequenceMixin[+Element <: Shape.Any] extends Mixin[Shape.Sequence[Element]] {
    def elements: Seq[Abstract[Element]]
  }

  trait OptionalMixin[+Value <: Shape.Concrete] extends Mixin[Shape.Optional[Value]] {
    def hasValue: Boolean
  }

  trait SomeMixin[+Value <: Shape.Concrete] extends OptionalMixin[Value] {
    def value: Abstract[Value]
    final override def hasValue = true
  }

  trait NoneMixin extends OptionalMixin[Shape.Nil] {
    final override def hasValue = false
  }

  type RichPrimitive[+RawType] <: PrimitiveMixin[RawType] with Primitive[RawType]
  type RichStruct <: StructMixin with Struct
  type RichTuple <: TupleMixin with Tuple
  type RichTuple1[+Coordinate1 <: Shape.Any] <: Tuple1Mixin[Coordinate1] with Tuple1[Coordinate1]
  type RichTuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] <: Tuple2Mixin[Coordinate1, Coordinate2] with Tuple2[Coordinate1, Coordinate2]
  type RichSequence[+Element <: Shape.Any] <: SequenceMixin[Element] with Sequence[Element]
  type RichOptional[+Value <: Shape.Concrete] <: OptionalMixin[Value] with Optional[Value]
  type RichSome[+Value <: Shape.Concrete] <: SomeMixin[Value] with Some[Value]
  type RichNone <: NoneMixin with None
}