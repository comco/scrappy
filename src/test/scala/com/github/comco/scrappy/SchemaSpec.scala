package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class SchemaSpec extends FlatSpec with Matchers {
  val int = Schema.Primitive[Int]
  val string = Schema.Primitive[String]
  val boolean = Schema.Primitive[Boolean]

  "A Schema.Primitive" should "provide typeTag" in {
    int.typeTag shouldEqual implicitly[TypeTag[Int]]
  }

  val lineStruct = Schema.Struct("line", "number" -> int, "content" -> string)
  val pageStruct = Schema.Struct("page", "number" -> int, "lines" -> Schema.Sequence(lineStruct))

  "A Schema.Struct" should "be constructible from a Map" in {
    Schema.Struct("line", Map("number" -> int, "content" -> string)) shouldEqual lineStruct
  }

  it should "provide name" in {
    lineStruct.name shouldEqual "line"
  }

  it should "provide featureSchemas" in {
    lineStruct.featureSchemas shouldEqual Map("number" -> int, "content" -> string)
  }

  it should "provide hasFeatureNamed" in {
    lineStruct.hasFeatureNamed("number") shouldEqual true
    lineStruct.hasFeatureNamed("content") shouldEqual true
    lineStruct.hasFeatureNamed("nonexistent") shouldEqual false
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

  it should "provide valueSchema" in {
    optional.valueSchema shouldEqual int
  }
  
  "A Schema.None" should "provide valueSchema" in {
    Schema.None.valueSchema shouldEqual Schema.Nil
  }
  
  "A Schema" should "provide satisfy" in {
    int.satisfies(int) shouldEqual true
    int.satisfies(string) shouldEqual false
    int.satisfies(tuple) shouldEqual false
    int.satisfies(lineStruct) shouldEqual false
    int.satisfies(Schema.Any) shouldEqual true
    Schema.Nil.satisfies(int) shouldEqual true
    Schema.None.satisfies(optional) shouldEqual true
  }
}