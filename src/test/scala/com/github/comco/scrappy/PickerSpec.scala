package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.github.comco.scrappy.PrimitiveType._
import DataDomain.PrimitiveData._
import DataDomain.PrimitiveData
import OriginatedDataDomain.{mkOriginatedDataFrom, mkDataOriginatedFromSelf}
import com.github.comco.scrappy.DataDomain.NoneData

class PickerSpec extends FlatSpec {
	val data = DataDomain.PrimitiveData("hi")
	val selfPicker = SelfPicker(IntPrimitiveType)
	
  "A SelfPicker" should "have the same targetType as its sourceType" in {
    selfPicker.targetType shouldEqual IntPrimitiveType
  }
  
  it should "pick data of the valid datatype unchanged" in {
    SelfPicker(StringPrimitiveType).pickData(data) shouldEqual data
  }
  
  it should "throw an exception when pickData is called with data of invalid datatype" in {
    an[IllegalArgumentException] should be thrownBy
      SelfPicker(IntPrimitiveType).pickData(data)
  }
  
  val tupleData = DataDomain.TupleData(3, 4)
  val tupleType = tupleData.datatype
  val coordinatePicker = CoordinatePicker(tupleData.datatype, 0)
  
  "A CoordinatePicker" should "have the right sourceType" in {
    coordinatePicker.sourceType shouldEqual tupleType
  }
  
  it should "have the right targetType" in {
    coordinatePicker.targetType shouldEqual IntPrimitiveType
  }
  
  it should "validate the datatype in pickData" in {
    an[IllegalArgumentException] should be thrownBy coordinatePicker.pickData(data)
  }
  
  it should "pick the right result" in {
    coordinatePicker.pickData(tupleData) shouldEqual PrimitiveData(3)
  }
  
  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(tupleType))
    val originated = mkDataOriginatedFromSelf(tupleData)
    val expected = mkOriginatedDataFrom(PrimitiveData(3), origin.append(CoordinateStep(tupleType, 0)))
    coordinatePicker.pickOriginatedData(originated) shouldEqual expected
  }
  
  it should "validate against wrong coordinates" in {
    an[IllegalArgumentException] should be thrownBy CoordinatePicker(tupleData.datatype, -1)
    an[IllegalArgumentException] should be thrownBy CoordinatePicker(tupleData.datatype, 3)
  }
  
  val structType = StructType("struct", "a" -> tupleType)
  val structData = DataDomain.StructData(structType)("a" -> tupleData)
  val featurePicker = FeaturePicker(structType, "a")
  
  "A FeaturePicker" should "have the right sourceType" in {
    featurePicker.sourceType shouldEqual structType
  }
  
  it should "have the right targetType" in {
    featurePicker.targetType shouldEqual tupleType
  }
  
  it should "pick data" in {
    featurePicker.pickData(structData) shouldEqual tupleData
  }
  
  it should "validate for correct feature name" in {
    an[IllegalArgumentException] should be thrownBy FeaturePicker(structType, "b")
  }
  
  it should "validate datatype in pickData" in {
    an[IllegalArgumentException] should be thrownBy featurePicker.pickData(tupleData)
  }
  
  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(structType))
    val originated = mkDataOriginatedFromSelf(structData)
    val expected = mkOriginatedDataFrom(tupleData, origin.append(FeatureStep(structType, "a")))
    featurePicker.pickOriginatedData(originated) shouldEqual expected
  }
  
  val seqType = SeqType(structType)
  val seqData = DataDomain.SeqData(structData, structData)
  val elementPicker = ElementPicker(seqType, 1)
  
  "An ElementPicker" should "have the right sourceType" in {
    elementPicker.sourceType shouldEqual seqType
  }
  
  it should "have the right targetType" in {
    elementPicker.targetType shouldEqual structType
  }
  
  it should "pick data" in {
    elementPicker.pickData(seqData) shouldEqual structData
  }
  
  it should "validate indices during picking data" in {
    val longElementPicker = ElementPicker(seqType, 10)
    an[IndexOutOfBoundsException] should be thrownBy longElementPicker.pickData(seqData)
  }
  
  it should "validate for correct index" in {
    an[IllegalArgumentException] should be thrownBy ElementPicker(seqType, -1)
  }
  
  it should "validate datatype in pickData" in {
    an[IllegalArgumentException] should be thrownBy elementPicker.pickData(tupleData)
  }
  
  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(seqType))
    val originated = mkDataOriginatedFromSelf(seqData)
    val expected = mkOriginatedDataFrom(structData, origin.append(ElementStep(seqType, 1)))
    elementPicker.pickOriginatedData(originated) shouldEqual expected
  }
  
  val optionType = OptionType(seqType)
  val optionData = DataDomain.SomeData(optionType, seqData)
  val somePicker = SomePicker(optionType)
  
  "A SomePicker" should "have the right sourceType" in {
    somePicker.sourceType shouldEqual optionType
  }
  
  it should "have the right targetType" in {
    somePicker.targetType shouldEqual seqType
  }
  
  it should "pickData" in {
    somePicker.pickData(optionData) shouldEqual seqData
  }
  
  it should "verify when none data is given" in {
    an[IllegalArgumentException] should be thrownBy
      somePicker.pickData(DataDomain.NoneData(optionType))
  }
  
  it should "verify that the appropriate data is given" in {
    an[IllegalArgumentException] should be thrownBy
      somePicker.pickData(seqData)
  }
  
  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(optionType))
    val originated = mkDataOriginatedFromSelf(optionData)
    val expected = mkOriginatedDataFrom(seqData, origin.append(SomeStep(optionType)))
    somePicker.pickOriginatedData(originated) shouldEqual expected
  }
  
  it should "validate for non-empty data in pickOriginatedData" in {
    an[IllegalArgumentException] should be thrownBy
      somePicker.pickOriginatedData(mkDataOriginatedFromSelf(NoneData(optionType)))
  }
  
  val andThenPicker = AndThenPicker(somePicker, elementPicker)
  
  "An AndThenPicker" should "have the right sourceType" in {
    andThenPicker.sourceType shouldEqual optionType
  }
  
  it should "have the right targetType" in {
    andThenPicker.targetType shouldEqual structType
  }
  
  it should "validate the parent and child pickers which create it" in {
    an[IllegalArgumentException] should be thrownBy AndThenPicker(somePicker, somePicker)
  }
  
  it should "pickData" in {
    andThenPicker.pickData(optionData) shouldEqual structData
  }
  
  it should "validate input datatype of pickData" in {
    an[IllegalArgumentException] should be thrownBy andThenPicker.pickData(structData)
  }
  
  it should "pickOriginatedData" in {
    val origin = OriginalOrigin(SelfPointer(optionType))
    val originated = mkDataOriginatedFromSelf(optionData)
    val expected = mkOriginatedDataFrom(structData,
      origin.append(SomeStep(optionType)).append(ElementStep(seqType, 1)))
    andThenPicker.pickOriginatedData(originated) shouldEqual expected
  }
}