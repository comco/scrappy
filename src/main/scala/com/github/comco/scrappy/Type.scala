package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

/**
 * Base class for scrappy types.
 *
 * Types have a lattice structure.
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

  def isSubtypeOf(that: Type.Any) = that match {
    // Top type is a supertype of anything
    case Type.Any => true
    case _ => this match {
      // Bottom type is a subtype of anything
      case Type.Nil => true
      case _ => (this == that)
    }
  }

  def join(that: Type.Any): Type.Any = {
    if (this == Type.Nil) that
    else if (that == Type.Nil) this
    else if (this == that) this
    else Type.Any
  }

  def meet(that: Type.Any): Type.Any = {
    if (this == Type.Any) that
    else if (that == Type.Any) this
    else if (this == that) this
    else Type.Nil
  }

  def compatibleWith(that: Type.Any) =
    (this meet that) != Type.Nil
}

object Type extends Domain {
  type Abstract[+Shape <: Shape.Any] = Type[Shape]

  def join(types: Seq[Any]) = ((Nil: Type.Any) /: types)(_.join(_))
  def meet(types: Seq[Any]) = ((Any: Type.Any) /: types)(_.meet(_))

  object Any extends Any
  object Nil extends Nil

  /**
   * Primitive scrappy types.
   */
  case class RichPrimitive[+RawType: TypeTag]() extends Primitive[RawType]

  /**
   * Tuple type is for data having coordinates. The coordinates can be indexed by
   * position. The position is zero-based.
   */
  sealed abstract class RichTuple extends Tuple {
    def coordinateTypes: IndexedSeq[Type.Any]

    /**
     * The number of coordinates of this tuple type.
     */
    def length: Int = coordinateTypes.length

    /**
     * Checks if this tuple type contains a coordinate at some position.
     */
    def hasCoordinate(position: Int): Boolean =
      (0 <= position && position < coordinateTypes.length)

    /**
     * Retrieves the type of the coordinate of this tuple type at some position.
     */
    def coordinateType(position: Int): Type.Any = {
      require(hasCoordinate(position),
        s"Invalid coordinate position: $position for a TupleType: $this.")

      coordinateTypes(position)
    }
  }

  /**
   * Tuple type is for data having coordinates. The coordinates can be indexed by
   * position. The position is zero-based.
   */
  case class RichTupleN(val coordinateTypes: IndexedSeq[Type.Any]) extends Tuple

  case class RichTuple1[+Coordinate1 <: Shape.Any: TypeTag](val coordinate1Type: Type[Coordinate1]) extends RichTuple with Tuple1[Coordinate1] {
    def coordinateTypes = IndexedSeq(coordinate1Type)
  }

  case class RichTuple2[+Coordinate1 <: Shape.Any: TypeTag, +Coordinate2 <: Shape.Any: TypeTag](val coordinate1Type: Type[Coordinate1], val coordinate2Type: Type[Coordinate2]) extends RichTuple with Tuple2[Coordinate1, Coordinate2] {
    def coordinateTypes = IndexedSeq(coordinate1Type, coordinate2Type)
  }

  object Tuple {
    /**
     * Constructs a tuple type from a series of types representing the coordinate
     * types.
     */
    def apply(coordinateTypes: Type.Any*) = RichTupleN(coordinateTypes.toIndexedSeq)

    def apply[Coordinate1 <: Shape.Any: TypeTag](coordinate1Type: Type[Coordinate1]) = RichTuple1(coordinate1Type)

    def apply[Coordinate1 <: Shape.Any: TypeTag, Coordinate2 <: Shape.Any: TypeTag](coordinate1Type: Type[Coordinate1], coordinate2Type: Type[Coordinate2]) = RichTuple2(coordinate1Type, coordinate2Type)
  }

  case class RichStruct(val name: String, val featureTypes: Map[String, Type.Any]) extends Struct {
    /**
     * The size (number of features) of this struct type.
     */
    def size: Int = featureTypes.size

    /**
     * Checks if this struct type has a feature with some name.
     */
    def hasFeature(name: String): Boolean = featureTypes.contains(name)

    /**
     * Retrieves the feature type of a feature with some name from this struct
     * type.
     */
    def featureType(name: String): Type.Any = {
      require(hasFeature(name),
        s"Invalid feature name: $name for a StructType: $this.")

      featureTypes(name)
    }
  }

  object Struct {
    /**
     * Constructs a struct type with some name and a sequence of named feature
     * types.
     */
    def apply(name: String, featureTypes: (String, Type.Any)*) = {
      RichStruct(name, featureTypes.toMap)
    }
  }

  case class RichSequence[+Element <: Shape.Any](val elementType: Type[Element]) extends Sequence[Element]

  object Sequence {
    def apply[Element <: Shape.Any](elementType: Type[Element]) = RichSequence(elementType)
  }

  sealed abstract class RichOptional[+Value <: Shape.Concrete] extends Optional[Value] {
    def hasValue: Boolean
  }

  case class RichSome[+Value <: Shape.Concrete](val valueType: Type[Value]) extends RichOptional[Value] with Some[Value] {
    def hasValue = true
  }

  sealed abstract class RichNone extends RichOptional[Shape.Nil] with None

  object None extends RichNone {
    def hasValue = false
  }
}