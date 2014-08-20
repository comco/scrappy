package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import scala.collection.mutable.Stack
import com.github.comco.scrappy.PrimitiveType._

class TupleTypeSpec extends FlatSpec {
  val a = BooleanPrimitiveType
  val b = StringPrimitiveType
  val c = IntPrimitiveType
  val tt = TupleType(a, b, c)

  "A TupleType" should "return its size by the size member" in {
    tt.size shouldEqual 3
  }

  it should "return each of its coordinate types by the coordinateType member" in {
    (0 until 3).map(tt.coordinateType(_)) shouldEqual Seq(a, b, c)
  }

  it should "verify that the given position is correct" in {
    an[IllegalArgumentException] should be thrownBy tt.coordinateType(3)
  }

  it should "support quering for coordinate existence" in {
    (0 to 3).map(tt.hasCoordinate(_)) shouldEqual Seq(true, true, true, false)
  }

  it should "have the right coordinateTypes member" in {
    tt.coordinateTypes shouldEqual Seq(a, b, c)
  }
}

class StructTypeSpec extends FlatSpec {
  val a = PrimitiveType.IntPrimitiveType
  val b = PrimitiveType.StringPrimitiveType
  val str = StructType("name", "f1" -> a, "f2" -> b, "f3" -> a)

  "A StructType" should "have size" in {
    str.size shouldEqual 3
  }

  it should "return the right feature type" in {
    str.featureType("f1") shouldEqual a
  }

  it should "throw an exception when the feature type is not correct" in {
    an[IllegalArgumentException] should be thrownBy str.featureType("non-existing")
  }

  it should "have name" in {
    str.name shouldEqual "name"
  }

  it should "have all the feature types" in {
    str.featureTypes shouldEqual Map("f1" -> a, "f2" -> b, "f3" -> a)
  }

  it should "support querying for feature names" in {
    str.hasFeature("f3") shouldBe true
    str.hasFeature("none") shouldBe false
  }
}

class SeqTypeSpec extends FlatSpec {
  val seq = SeqType(IntPrimitiveType)
  
  "A SeqType" should "have elementType" in {
    seq.elementType shouldEqual IntPrimitiveType
  }
}

class OptionTypeSpec extends FlatSpec {
  "An OptionType" should "provide a someType" in {
    OptionType(IntPrimitiveType).someType shouldEqual IntPrimitiveType
  }
  
  it should "be flat and don't allow creating nested option types" in {
    val opt = OptionType(IntPrimitiveType)
    an[IllegalArgumentException] should be thrownBy OptionType(opt)
  }
}