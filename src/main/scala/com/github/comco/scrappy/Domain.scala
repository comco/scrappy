package com.github.comco.scrappy

import scala.language.higherKinds
import scala.reflect.runtime.universe.TypeTag

abstract class Domain {
  type Base[+Shape >: Shape.Nil <: Shape.Any] <: DomainBase[Shape]

  type Any = Base[Shape.Any]
  type Concrete = Base[Shape.Concrete]
  type Primitive[+RawType] <: Base[Shape.Primitive[RawType]]
  type Struct <: Base[Shape.Struct]
  type Tuple <: Base[Shape.Tuple]
  type Seq[+ElementShape <: Shape.Any] <: Base[Shape.Seq[ElementShape]]
  type Optional[+ValueShape <: Shape.Concrete] <: Base[Shape.Optional[ValueShape]]
  type Some[+ValueShape <: Shape.Concrete] <: Optional[ValueShape]
  type None <: Optional[Shape.Nil]

  abstract class DomainBase[+Shape >: Shape.Nil <: Shape.Any] {
    self: Base[Shape] =>
  }

  abstract class DomainPrimitive[+RawType: TypeTag] extends DomainBase[Shape.Primitive[RawType]] {
    self: Primitive[RawType] =>
    def raw: RawType
  }

  abstract class DomainStruct extends DomainBase[Shape.Struct] {
    self: Struct =>
    def name: String
    def features: Map[String, Any]
  }

  abstract class DomainTuple extends DomainBase[Shape.Tuple] {
    self: Tuple =>
    def coordinates: IndexedSeq[Any]
  }
}