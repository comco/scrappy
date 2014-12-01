package com.github.comco.scrappy

import scala.reflect.runtime.universe.{ _ => u }
import scala.language.implicitConversions

/**
 * Base class for scrappy types.
 */
sealed trait Type[+Shape <: Shape.Any] {
  def shapeType: scala.reflect.runtime.universe.Type

  /**
   * Defines a subtype relation.
   */
  def <:<(that: Type.Any): Boolean = {
    val shapesCompatible = (this.shapeType <:< that.shapeType)
    val shapesDifferent = !(this.shapeType =:= that.shapeType)
    val instancesEqual = (this == that)
    return shapesCompatible && (shapesDifferent || instancesEqual)
  }
}

object Type extends Domain {
  type Abstract[+Shape <: Shape.Any] = Type[Shape]

  case class RichPrimitive[+RawType: TypeTag]()
      extends Primitive[RawType] {
    def shapeType = typeOf[Shape.Primitive[RawType]]
  }

  case class RichStruct(val name: String, val featureTypes: Map[String, Any])
      extends Struct {
    def shapeType = typeOf[Shape.Struct]
  }

  sealed abstract class RichTuple extends Tuple {
    def shapeType = typeOf[Shape.Tuple]
    def coordinateTypes: IndexedSeq[Any]
  }

  case class RichTuple1[+Coordinate1 <: Shape: TypeTag](val coordinate1Type: Type[Coordinate1])
      extends RichTuple with Tuple1[Coordinate1] {
    def coordinateTypes = IndexedSeq(coordinate1Type)
  }

  case class RichTuple2[+Coordinate1 <: Shape: TypeTag, +Coordinate2 <: Shape: TypeTag](
    val coordinate1Type: Type[Coordinate1],
    val coordinate2Type: Type[Coordinate2])
      extends RichTuple with Tuple2[Coordinate1, Coordinate2] {
    def coordinateTypes = IndexedSeq(coordinate1Type, coordinate2Type)
  }

  case class RichSequence[+Element <: Shape: TypeTag](val elementType: Type[Element])
      extends Sequence[Element] {
    def shapeType = typeOf[Shape.Sequence[Element]]
  }

  sealed abstract class RichOptional[+Value <: Shape.Concrete: TypeTag]
      extends Optional[Value] {
    def shapeType = typeOf[Shape.Optional[Value]]
    def valueType: Type[Value]
  }

  sealed abstract class RichSome[+Value <: Shape.Concrete: TypeTag](val valueType: Abstract[Value])
    extends RichOptional[Value] with Some[Value]

  sealed abstract class RichNone extends RichOptional[Nothing] with None {
    val valueType = Nil
  }

  object RichNone extends RichNone

  object Any extends Any {
    val shapeType = typeOf[Shape.Any]
  }

  object Nil extends Nil {
    val shapeType = typeOf[Shape.Nil]
  }
}