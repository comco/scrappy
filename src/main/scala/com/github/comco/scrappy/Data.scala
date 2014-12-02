package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

sealed trait Data[+Shape <: Shape.Any] {
  def datatype: Type[Shape]
}

object Data extends Domain {
  type Abstract[+Shape <: Shape.Any] = Data[Shape]

  abstract class RichPrimitive[+RawType: TypeTag] extends Primitive[RawType] {
    def raw: RawType

    override def datatype = Type.Primitive[RawType]
  }

  abstract class RichStruct extends Struct {
    def features: Map[String, Data.Any]
  }

  abstract class RichTuple extends Tuple {
    def coordinates: IndexedSeq[Data.Any]
  }

  abstract class RichTuple1[+Coordinate1 <: Shape.Any: TypeTag] extends RichTuple {
    def coordinate1: Data[Coordinate1]

    override def coordinates = IndexedSeq(coordinate1)
  }

  abstract class RichTuple2[+Coordinate1 <: Shape.Any: TypeTag, +Coordinate2 <: Shape.Any: TypeTag] extends RichTuple {
    def coordinate1: Data[Coordinate1]
    def coordinate2: Data[Coordinate2]

    override def coordinates = IndexedSeq(coordinate1, coordinate2)
  }

  abstract class RichSequence[+Element <: Shape.Any: TypeTag] extends Sequence[Element] {
    def elements: Seq[Data[Element]]
  }

  abstract class RichOptional[+Value <: Shape.Concrete: TypeTag] extends Optional[Value] {
    def hasValue: Boolean
  }

  abstract class RichSome[+Value <: Shape.Concrete: TypeTag] extends RichOptional[Value] with Some[Value] {
    def value: Data[Value]

    override def hasValue = true
  }

  abstract class RichNone extends RichOptional[Shape.Nil] with None {
    override def hasValue = false
  }

  trait Factory {
    def primitive[RawType: TypeTag](raw: RawType): RichPrimitive[RawType]
  }
}