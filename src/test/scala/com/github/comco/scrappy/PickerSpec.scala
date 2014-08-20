package com.github.comco.scrappy

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType

class PickerSpec extends FlatSpec {
	val data = DataDomain.PrimitiveData("hi")
	
	"A BaseTuplePicker" should "validate its source datatype" in {
	  
	}
	
  "A SelfPicker" should "have the same targetType as its sourceType" in {
    SelfPicker(IntPrimitiveType).targetType shouldEqual IntPrimitiveType
  }
  
  it should "pick data of the valid datatype unchanged" in {
    SelfPicker(StringPrimitiveType).pickData(data) shouldEqual data
  }
  
  it should "throw an exception when pickData is called with data of invalid datatype" in {
    an[IllegalArgumentException] should be thrownBy
      SelfPicker(IntPrimitiveType).pickData(data)
  }
}