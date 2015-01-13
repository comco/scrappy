package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.github.comco.scrappy.schema.basic.BasicFactory

class SchemaSpec extends FlatSpec with Matchers {
  implicit def schemaFactory = BasicFactory

  val int = Schema.Primitive[Int]
  val string = Schema.Primitive[String]
  val boolean = Schema.Primitive[Boolean]

  "A Schema.Primitive" should "provide typeTag" in {
    int.typeTag shouldEqual implicitly[TypeTag[Int]]
  }

  val struct = Schema.Struct("line", "number" -> int, "content" -> string)

  "A Schema.Struct" should "be constructible from a Map" in {
    Schema.Struct("line", Map("number" -> int, "content" -> string)) shouldEqual struct
  }

  it should "provide name" in {
    struct.name shouldEqual "line"
  }

  it should "provide featureSchemas" in {
    struct.featureSchemas shouldEqual Map("number" -> int, "content" -> string)
  }

  it should "provide hasFeatureNamed" in {
    struct.hasFeatureNamed("number") shouldEqual true
    struct.hasFeatureNamed("content") shouldEqual true
    struct.hasFeatureNamed("nonexistent") shouldEqual false
  }

  val tuple = Schema.Tuple(int, string, int, boolean)

  "A Schema.Tuple" should "be constructible from an IndexedSeq" in {
    Schema.Tuple(IndexedSeq(int, string, int, boolean)) shouldEqual tuple
  }

  it should "provide arity" in {
    tuple.arity shouldEqual 4
  }

  it should "provide coordinateSchemas" in {
    tuple.coordinateSchemas shouldEqual IndexedSeq(int, string, int, boolean)
  }

  it should "provide hasCoordinateAtPosition" in {
    tuple.hasCoordinateAtPosition(-1) shouldEqual false
    tuple.hasCoordinateAtPosition(0) shouldEqual true
    tuple.hasCoordinateAtPosition(1) shouldEqual true
    tuple.hasCoordinateAtPosition(2) shouldEqual true
    tuple.hasCoordinateAtPosition(3) shouldEqual true
    tuple.hasCoordinateAtPosition(4) shouldEqual false
  }

  val sequence = Schema.Sequence(tuple)

  "A Schema.Sequence" should "provide elementSchema" in {
    sequence.elementSchema shouldEqual tuple
  }

  val optional = Schema.Optional(int)

  "A Schema.Optional" should "provide hasValue" in {
    optional.hasValue shouldEqual true
  }

  it should "provide valueSchema" in {
    optional.valueSchema shouldEqual int
  }

  "A Schema.None" should "provide hasValue" in {
    schemaFactory.none.hasValue shouldEqual false
  }

  it should "provide valueSchema" in {
    schemaFactory.none.valueSchema.isInstanceOf[Schema.RichDynamic] shouldEqual true
  }
}