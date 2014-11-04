package com.github.comco.scrappy

import org.scalatest.FlatSpec

import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType

final class PrimitiveTypeSpec extends FlatSpec with CustomMatchers {
  "A PrimitiveType" should "contain type names" in {
    PrimitiveType.typeNames("int") shouldEqual IntPrimitiveType
    PrimitiveType.typeNames("string") shouldEqual StringPrimitiveType
    PrimitiveType.typeNames("boolean") shouldEqual BooleanPrimitiveType
  }
}

final class TupleTypeSpec extends FlatSpec with CustomMatchers {
  val booleanType = BooleanPrimitiveType
  val stringType = StringPrimitiveType
  val intType = IntPrimitiveType
  val tupleType = TupleType(booleanType, stringType, intType)

  "A TupleType" should "provide length" in {
    tupleType.length shouldEqual 3
  }

  it should "provide the type of a coordinate at a position by coordinateType" in {
    (0 until 3).map(tupleType.coordinateType(_)) shouldEqual
      Seq(booleanType, stringType, intType)
  }

  it should "check if the position passed to coordinateType is valid" in {
    itShouldBeDisallowed calling tupleType.coordinateType(3)
  }

  it should "support checking for a valid coordinate position by hasCoordinate" in {
    (0 to 3).map(tupleType.hasCoordinate(_)) shouldEqual
      Seq(true, true, true, false)
  }

  it should "provide coordinateTypes" in {
    tupleType.coordinateTypes shouldEqual Seq(booleanType, stringType, intType)
  }
}

class StructTypeSpec extends FlatSpec with CustomMatchers {
  val intType = PrimitiveType.IntPrimitiveType
  val stringType = PrimitiveType.StringPrimitiveType
  val structType = StructType("name",
    "f1" -> intType,
    "f2" -> stringType,
    "f3" -> intType)

  "A StructType" should "provide size" in {
    structType.size shouldEqual 3
  }

  it should "provide the feature type of a feature with a name by featureType" in {
    structType.featureType("f1") shouldEqual intType
  }

  it should "check if the name passed to featureType is valid" in {
    itShouldBeDisallowed calling structType.featureType("non-existing")
  }

  it should "provide name" in {
    structType.name shouldEqual "name"
  }

  it should "provide featureTypes" in {
    structType.featureTypes shouldEqual
      Map("f1" -> intType, "f2" -> stringType, "f3" -> intType)
  }

  it should "support checking for a valid feature name by hasFeature" in {
    structType.hasFeature("f3") shouldBe true
    structType.hasFeature("none") shouldBe false
  }
}

class SeqTypeSpec extends FlatSpec with CustomMatchers {
  val seqType = SeqType(IntPrimitiveType)

  "A SeqType" should "provide elementType" in {
    seqType.elementType shouldEqual IntPrimitiveType
  }
}

class OptionTypeSpec extends FlatSpec with CustomMatchers {
  val optionType = OptionType(IntPrimitiveType)

  "An OptionType" should "provide someType" in {
    optionType.someType shouldEqual IntPrimitiveType
  }

  "An OptionType during construction" should "not allow creating nested option types" in {
    itShouldBeDisallowed calling OptionType(optionType)
  }
}