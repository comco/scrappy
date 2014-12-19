package com.github.comco.scrappy

import scala.language.implicitConversions
import scala.reflect.runtime.universe._

import com.github.comco.scrappy.data._

/**
 * Base class for scrappy Data.
 * Represents a piece of immutable, typed and interchangable data.
 * This data is intended to be independent of the specific representation,
 * so the equals methods of the derivable subclasses are final.
 */
sealed trait Data[+Shape <: Shape.Any] {
  /**
   * The type of this data.
   */
  def datatype: Type[Shape]
}

object Data extends Domain {
  type Abstract[+Shape <: Shape.Any] = Data[Shape]

  /**
   * Checks if data contains a value.
   * OptionData in case of none data doesn't contain a value.
   */
  def isFilled(data: Data.Any): Boolean = data match {
    case data: RichOptional[_] => data.hasValue
    case _ => true
  }

  /**
   * Checks if data can be assigned to a field of type datatype.
   * An option data field can be assigned by its corresponding value type.
   */
  def canAssign(datatype: Type.Any, data: Data.Any): Boolean = {
    data.datatype == datatype ||
      (datatype.isInstanceOf[Type.RichSome[_]] &&
        datatype.asInstanceOf[Type.RichSome[_]].valueType == data.datatype)
  }

  /**
   * Converts data to a directly assignable to datatype value.
   */
  def convert(datatype: Type.Any, data: Data.Any): Data.Any = {
    assert(canAssign(datatype, data))
    if (data.datatype == datatype) {
      data
    } else {
      // TODO: BaseSome(datatype.asInstanceOf[Type.BaseSome[_]], data)
      ???
    }
  }

  abstract class RichPrimitive[+RawType: TypeTag] extends Primitive[RawType] {
    def datatype: Primitive[RawType]

    /**
     * The raw value of this primitive data.
     */
    def value: RawType

    private def state = (datatype, value)

    final override def equals(that: Any) = that match {
      case that: RichPrimitive[RawType] => this.state == that.state
      case _ => false
    }

    final override def hashCode() = state.hashCode()
  }

  object Primitive {
    def apply[RawType](value: RawType) =
      // TODO: SimplePrimitiveData(value)
      ???
  }

  sealed abstract class RichOptional[+Value <: Shape.Concrete: TypeTag] extends Optional[Value] {
    def hasValue: Boolean
  }

  abstract class RichNone extends RichOptional[Shape.Nil] with None {
    final def hasValue = false

    private def state = (datatype)

    final override def equals(that: scala.Any) = that match {
      case that: RichNone => this.state == that.state
      case _ => false
    }

    final override def hashCode() = state.hashCode()
  }

  object None extends RichNone

  abstract class RichSome[+Value <: Shape.Concrete: TypeTag] extends RichOptional[Value] with Some[Value] {
    final def hasValue = true

    /**
     * The value of this some data.
     */
    def value: Data[Value]

    private def state = (datatype, value)

    final override def equals(that: scala.Any) = that match {
      case that: RichSome[Value] => this.state == that.state
      case _ => false
    }

    final override def hashCode() = state.hashCode()
  }

  object Some {
    def apply[Value <: Shape.Concrete: TypeTag](value: Data[Value]) = DefaultSomeData(value)
  }

  abstract class RichTuple extends Tuple {
    def coordinates: IndexedSeq[Data.Any]

    def length: Int = coordinates.length

    /**
     * Checks if this tuple data has a coordinate at some position.
     * Positions of optional coordinates which are not filled-in
     * are regarded as not occupied.
     */
    def isOccupied(position: Int): Boolean = {
      datatype.hasCoordinate(position) && Data.isFilled(coordinates(position))
    }

    /**
     * Retrieves the coordinate of this data at some position.
     */
    def coordinate(position: Int): Data.Any = {
      require(datatype.hasCoordinate(position),
        s"Coordinate position: $position is out of bounds for TupleType: $datatype")

      coordinates(position)
    }

    private def state = (datatype, coordinates)

    final override def equals(that: scala.Any) = that match {
      case that: RichTuple => this.state == that.state
      case _ => false
    }

    final override def hashCode() = state.hashCode()
  }

  abstract class RichTuple1[+Coordinate1 <: Shape.Any: TypeTag] extends RichTuple with Tuple1[Coordinate1] {
    def coordinate1: Data[Coordinate1]

    override lazy val coordinates = IndexedSeq(coordinate1)
  }

  abstract class RichTuple2[+Coordinate1 <: Shape.Any: TypeTag, +Coordinate2 <: Shape.Any: TypeTag] extends RichTuple with Tuple2[Coordinate1, Coordinate2] {
    def coordinate1: Data[Coordinate1]
    def coordinate2: Data[Coordinate2]

    override lazy val coordinates = IndexedSeq(coordinate1, coordinate2)
  }

  object Tuple {
    def apply(coordinates: Data.Any*): Tuple = ???

    def apply[Coordinate1 <: Shape.Any: TypeTag](coordinate1: Data[Coordinate1]): Tuple1[Coordinate1] = ???

    def apply[Coordinate1 <: Shape.Any: TypeTag, Coordinate2 <: Shape.Any: TypeTag](coordinate1: Data[Coordinate1], coordinate2: Data[Coordinate2]): Tuple2[Coordinate1, Coordinate2] = ???
  }

  abstract class RichSequence[+Element <: Shape.Any: TypeTag] extends Sequence[Element] {
    def elements: Seq[Data[Element]]

    /**
     * Checks if this struct data has a feature with some name.
     * Names of optional features which are not filled-in
     * are regarded as not occupied.
     */
    def isOccupied(index: Int): Boolean = {
      0 <= index && index < length && Data.isFilled(elements(index))
    }

    /**
     * Retrieves the element of this seq data at some index.
     */
    def element(index: Int): Data[Element] = {
      require(0 <= index && index < length,
        s"Index: $index is out of bounds for SeqData with length: $length.")

      elements(index)
    }

    /**
     * The length of this seq data.
     */
    def length: Int = elements.length

    private def state = (datatype, elements)

    final override def equals(that: scala.Any) = that match {
      case that: RichSequence[Element] => this.state == that.state
      case _ => false
    }

    final override def hashCode() = state.hashCode()
  }

  object Sequence {
    def apply[Element <: Shape.Any: TypeTag](elements: Data[Element]*): Sequence[Element] = ???
  }

  abstract class RichStruct extends Struct {
    def features: Map[String, Data.Any]

    /**
     * Checks if this struct data has a feature with some name.
     * Names of optional features which are not filled-in
     * are regarded as not occupied.
     */
    def isOccupied(name: String): Boolean = {
      features.contains(name) && Data.isFilled(features(name))
    }

    /**
     * Retrieves the feature of this struct data with some name.
     */
    def feature(name: String): Data.Any = {
      require(datatype.hasFeature(name),
        s"StructData: $this doesn't contain a feature named: $name")

      features(name)
    }

    private def state = (datatype, features)

    final override def equals(that: scala.Any) = that match {
      case that: RichStruct => this.state == that.state
      case _ => false
    }

    final override def hashCode() = state.hashCode()
  }

  object Struct {
    def doApply(datatype: Type.Struct, features: Map[String, Data.Any]): Struct = ???

    def apply(datatype: Type.Struct, rawFeatures: Map[String, Data.Any]): Struct = {
      require(rawFeatures.keys.forall(datatype.hasFeature(_)), "Invalid feature name")
      require(rawFeatures.forall {
        case (name, data) => {
          Data.canAssign(datatype.featureType(name), data)
        }
      }, s"Invalid feature type for creating a StructData with datatype: $datatype from features: $rawFeatures")
      require(datatype.featureTypes.forall {
        case (name, datatype) => rawFeatures.contains(name) || datatype.isInstanceOf[Type.RichOptional[_]]
      }, "A non-optional feature is not given")

      val features: Map[String, Data.Any] = {
        val convertedFeatures: Map[String, Data.Any] = rawFeatures.transform({
          case (name, data) => Data.convert(datatype.featureType(name), data)
        })
        val missingFeatures: Map[String, Type.Any] = datatype.featureTypes.filter({
          case (name, _) => !rawFeatures.contains(name)
        })
        // all missing features should have option types
        convertedFeatures ++ missingFeatures.map {
          case (name, datatype) => (name, Data.None)
        }
      }
      doApply(datatype, features)
    }

    def apply(datatype: Type.Struct, features: (String, Data.Any)*): Data.Struct = {
      Struct(datatype, features.toMap)
    }
  }
}