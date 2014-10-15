package com.github.comco.scrappy.checker

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.picker.ConstPicker
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.PrimitiveData._
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.SelfPicker

class AndAlsoCheckerSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val firstChecker = EqualChecker(
      ConstPicker(tupleType, PrimitiveData(3)), 
      CoordinatePicker(tupleType, 0))
  val secondChecker = EqualChecker(
      ConstPicker(tupleType, PrimitiveData("hi")),
      CoordinatePicker(tupleType, 1))
  val thirdChecker = EqualChecker(SelfPicker(IntPrimitiveType), SelfPicker(IntPrimitiveType))
  val andChecker = AndAlsoChecker(firstChecker, secondChecker)  
  
  "An AndAlsoChecker" should "provide sourceType" in {
    andChecker.sourceType shouldEqual tupleType
  }
  
  val dataTrue = TupleData(3, "hi")
  val dataFalse1 = TupleData(4, "hi")
  val dataFalse2 = TupleData(3, "ho")
  
  it should "checkData" in {
    andChecker.checkData(dataTrue).successful shouldEqual true
    andChecker.checkData(dataFalse1).successful shouldEqual false
    andChecker.checkData(dataFalse2).successful shouldEqual false
  }
  
  it should "checkOriginatedData" in {
    
  }
  
  "An AndAlsoChecker during construction" should "check the source types of its arguments" in {
    itShouldBeDisallowed calling AndAlsoChecker(firstChecker, thirdChecker)
  }
}