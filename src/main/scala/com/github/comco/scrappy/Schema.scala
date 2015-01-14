package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy.utils.StateEquality

sealed trait Schema[+Shape <: Shape.Any] {
  def satisfies(that: Schema.Any) = Schema.satisfies(this, that)
}

object Schema extends Domain {
  def satisfies(a: Schema.Any, b: Schema.Any): Boolean = (a, b) match {
    case (_, Any) => true
    case (Nil, _) => true
    case (a: RichPrimitive[_], b: RichPrimitive[_]) => a.typeTag == b.typeTag
    case (a: RichStruct, b: RichStruct) => a == b
    case (a: RichTuple, b: RichTuple) => (a.arity == b.arity && 
        a.coordinateSchemas.zip(b.coordinateSchemas).forall {
      case (ac, bc) => satisfies(ac, bc)
    })
    case (a: RichSequence[_], b: RichSequence[_]) => satisfies(a.elementSchema, b.elementSchema)
    case (a: RichOptional[_], b: RichOptional[_]) => satisfies(a.valueSchema, b.valueSchema)
    case _ => false
  }
  
  def join[Shape <: Shape.Any](a: Schema[Shape], b: Schema[Shape]): Schema[Shape] = {
    if (a.satisfies(b)) b
    else if (b.satisfies(a)) a
    else Schema.Any.asInstanceOf[Schema[Shape]] // TODO: is this safe?
  }
  
  def meet[Shape <: Shape.Any](a: Schema[Shape], b: Schema[Shape]): Schema[Shape] = {
    if (a.satisfies(b)) a
    else if (b.satisfies(a)) b
    else Schema.Nil // TODO
  }
  
  type Abstract[+Shape <: Shape.Any] = Schema[Shape]

  case class RichPrimitive[Raw](implicit val typeTag: TypeTag[Raw])
      extends Primitive[Raw] with StateEquality[RichPrimitive[Raw]] {
    protected override def state = (typeTag)
  }
  
  case class RichStruct(val name: String, val featureSchemas: Map[String, Schema.Any])
      extends Struct with StateEquality[RichStruct] {

    def hasFeatureNamed(featureName: String): Boolean = featureSchemas.contains(featureName)

    protected override def state = (name, featureSchemas)
  }

  abstract class RichTuple extends Tuple with StateEquality[RichTuple] {
    def coordinateSchemas: IndexedSeq[Schema.Any]

    def arity = coordinateSchemas.length

    def hasCoordinateAtPosition(position: Int): Boolean = (0 <= position && position < arity)

    protected override def state = (coordinateSchemas)
  }

  case class RichTuple1[+Coordinate1 <: Shape.Any](val coordinate1Schema: Schema[Coordinate1])
      extends RichTuple with Tuple1[Coordinate1] {
    override def coordinateSchemas = IndexedSeq(coordinate1Schema)
  }

  case class RichTuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any](
    val coordinate1Schema: Schema[Coordinate1],
    val coordinate2Schema: Schema[Coordinate2])
      extends RichTuple with Tuple2[Coordinate1, Coordinate2] {
    override def coordinateSchemas = IndexedSeq(coordinate1Schema, coordinate2Schema)
  }
  
  case class RichTupleN(val coordinateSchemas: IndexedSeq[Schema.Any]) extends RichTuple

  case class RichSequence[+Element <: Shape.Any](val elementSchema: Schema[Element])
      extends Sequence[Element] with StateEquality[RichSequence[_]] {
    protected override def state = (elementSchema)
  }

  case class RichOptional[+Value <: Shape.Concrete](val valueSchema: Schema[Value])
      extends Optional[Value] with StateEquality[RichOptional[_]] {
    protected override def state = (valueSchema)
  }
  
  object Primitive {
    def apply[Raw: TypeTag] = RichPrimitive[Raw]
  }

  object Struct {
    def apply(name: String, featureSchemas: Map[String, Schema.Any]) = RichStruct(name, featureSchemas)

    def apply(name: String, featureSchemas: (String, Schema.Any)*) = RichStruct(name, featureSchemas.toMap)
  }

  object Tuple {
    def apply(coordinateSchemas: IndexedSeq[Schema.Any]) = RichTupleN(coordinateSchemas)

    def apply(coordinateSchemas: Schema.Any*) = RichTupleN(coordinateSchemas.toIndexedSeq)

    def apply[Coordinate1 <: Shape.Any](coordinate1Schema: Schema[Coordinate1]) = RichTuple1(coordinate1Schema)

    def apply[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
        coordinate1Schema: Schema[Coordinate1], 
        coordinate2Schema: Schema[Coordinate2]) = RichTuple2(coordinate1Schema, coordinate2Schema)
  }

  object Sequence {
    def apply[Element <: Shape.Any](elementSchema: Schema[Element]) = RichSequence(elementSchema)
  }

  object Optional {
    def apply[Value <: Shape.Concrete](valueSchema: Schema[Value]) = RichOptional(valueSchema)
  }
  
  val None = RichOptional(Nil)
  
  object Any extends Any
  
  object Nil extends Nil
}