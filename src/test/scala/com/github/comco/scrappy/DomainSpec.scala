package com.github.comco.scrappy

import com.github.comco.scrappy.PrimitiveType._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

object TestDomain extends Domain {
  abstract class Data extends BaseData

  case class PrimitiveData[T](val value: T)(implicit val datatype: PrimitiveType[T])
    extends Data with PrimitiveDataMixin[T]

  case class TupleData(val datatype: TupleType, val coordinates: IndexedSeq[Data])
    extends Data with TupleDataMixin

  case class StructData(val datatype: StructType, val features: Map[String, Data])
    extends Data with StructDataMixin

  case class SeqData(val datatype: SeqType, val elements: Seq[Data])
    extends Data with SeqDataMixin
    
  abstract class OptionData extends Data with OptionDataMixin
  
  case class SomeData(val datatype: OptionType, val value: Data) extends OptionData with SomeDataMixin
  
  case class NoneData(val datatype: OptionType) extends OptionData with NoneDataMixin
}

class DomainSpec extends FlatSpec {
  import TestDomain._

  val optionBooleanType = OptionType(BooleanPrimitiveType)
  
  val intData = PrimitiveData[Int](3)
  val stringData = PrimitiveData[String]("hi")
  val booleanData = PrimitiveData[Boolean](true)
  val someBooleanData = SomeData(optionBooleanType, booleanData)
  val noneBooleanData = NoneData(optionBooleanType)
  
  "A PrimitiveData" should "have the right datatype" in {
    Seq(intData, stringData, booleanData).map(_.datatype) shouldEqual
      Seq(IntPrimitiveType, StringPrimitiveType, BooleanPrimitiveType)
  }

  it should "have have the right data" in {
    Seq(intData, stringData, booleanData).map(_.value) shouldEqual Seq(3, "hi", true)
  }
  
  "An OptionData" should "have false isSome when None" in {
    NoneData(OptionType(IntPrimitiveType)).isSome shouldEqual false
  }
  
  it should "have true isSome when Some" in {
    SomeData(OptionType(IntPrimitiveType), intData).isSome shouldEqual true
  }
  
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType, OptionType(BooleanPrimitiveType))
  val tupleData = TupleData(tupleType, IndexedSeq(intData, stringData, someBooleanData))
  val tupleDataWithBlanks = TupleData(tupleType, IndexedSeq(intData, stringData, noneBooleanData))
  
  "A TupleData" should "have a TupleType datatype" in {
    tupleData.datatype shouldEqual tupleType
  }

  it should "have size" in {
    tupleData.size shouldEqual 3
    tupleDataWithBlanks.size shouldEqual 3
  }

  it should "support checking valid positions with hasCoordinate" in {
    (0 to 3) map (tupleData.hasCoordinate(_)) shouldEqual
      Seq(true, true, true, false)
    (0 to 3) map (tupleDataWithBlanks.hasCoordinate(_)) shouldEqual
      Seq(true, true, false, false)
  }

  it should "support extracting coordinates one by one" in {
    (0 until 3).map(tupleData.coordinate(_)) shouldEqual Seq(intData, stringData, someBooleanData)
    tupleDataWithBlanks.coordinate(2) shouldEqual noneBooleanData
  }
  
  it should "support extracting a sequence of the coordinates" in {
    tupleData.coordinates shouldEqual Seq(intData, stringData, someBooleanData)
    tupleDataWithBlanks.coordinates shouldEqual Seq(intData, stringData, noneBooleanData)
  }
  
  it should "throw an exception when an invalid position is given" in {
    an[IllegalArgumentException] should be thrownBy tupleData.coordinate(-1)
    an[IllegalArgumentException] should be thrownBy tupleData.coordinate(4)
  }
  
  val structType = StructType("name", "a" -> IntPrimitiveType, "b" -> StringPrimitiveType, "cd" -> optionBooleanType)
  val structData = StructData(structType,
      Map("a" -> PrimitiveData(3), "b" -> PrimitiveData("ala"), "cd" -> someBooleanData))
  val structDataWithBlanks = StructData(structType, 
      Map("a" -> PrimitiveData(5), "b" -> PrimitiveData("hi"), "cd" -> noneBooleanData))
  
  
  "A StructData" should "have a StructType datatype" in {
    structData.datatype shouldEqual structType
    structDataWithBlanks.datatype shouldEqual structType
  }
  
  it should "support checking feature names" in {
    Seq("a", "b", "cd", "none") map (structData.hasFeature(_)) shouldEqual
      Seq(true, true, true, false)
    Seq("a", "b", "cd", "none") map (structDataWithBlanks.hasFeature(_)) shouldEqual
      Seq(true, true, false, false)
  }
  
  it should "support extracting features by a valid name" in {
    Seq("a", "b", "cd") map (structData.feature(_)) shouldEqual
      Seq(PrimitiveData(3), PrimitiveData("ala"), someBooleanData)
  }
  
  it should "throw exception when an invalid feature name is given" in {
    an[IllegalArgumentException] should be thrownBy structData.feature("none")
    an[IllegalArgumentException] should be thrownBy structDataWithBlanks.feature("")
  }
  
  val seqType = SeqType(IntPrimitiveType)
  val elements = Seq(PrimitiveData(3), PrimitiveData(4))
  val seqValue = SeqData(seqType, elements)
  val seqOptionType = SeqType(optionBooleanType)
  val elementsWithBlanks = Seq(
      SomeData(optionBooleanType, PrimitiveData(true)),
      NoneData(optionBooleanType),
      SomeData(optionBooleanType, PrimitiveData(false)))
  val seqValueWithBlanks = SeqData(seqOptionType, elementsWithBlanks)
  
  "A SeqData" should "have a SeqType datatype" in {
    seqValue.datatype shouldEqual seqType
    seqValueWithBlanks.datatype shouldEqual seqOptionType
  }
  
  it should "support extracting a sequence of all the elements" in {
    seqValue.elements shouldEqual elements
    seqValueWithBlanks.elements shouldEqual elementsWithBlanks
  }
  
  it should "have the right length" in {
    seqValue.length shouldEqual 2
    seqValueWithBlanks.length shouldEqual 3
  }
  
  it should "allow checking for elements" in {
    Seq(0, 1, -1, 2) map(seqValue.hasElement(_)) shouldEqual
      Seq(true, true, false, false)
    
    seqValueWithBlanks.hasElement(1) shouldEqual false
  }
  
  it should "support extracting elements by a valid index" in {
    seqValue.element(0) shouldEqual PrimitiveData(3)
    seqValue.element(1) shouldEqual PrimitiveData(4)
    seqValueWithBlanks.element(0) shouldEqual elementsWithBlanks(0)
  }
  
  it should "throw an exception when invalid index is given" in {
    an[IllegalArgumentException] should be thrownBy seqValue.element(-1)
  }
}