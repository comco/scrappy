package com.github.comco.scrappy.pointer

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.picker.AndThenPicker
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.SeqType

final class PointerSpec extends FlatSpec with CustomMatchers {
  "A SelfPointer" should "have the right targetType" in {
    SelfPointer(IntPrimitiveType).targetType shouldEqual IntPrimitiveType
  }

  it should "have a SelfPicker" in {
    SelfPointer(IntPrimitiveType).picker shouldEqual SelfPicker(IntPrimitiveType)
  }

  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)

  "A StepPointer" should "have the right targetType" in {
    val ptr = StepPointer(SelfPointer(tupleType), CoordinateStep(tupleType, 1))
    ptr.targetType shouldEqual StringPrimitiveType
  }

  it should "validate its step during construction" in {
    val structType = StructType("name", "a" -> BooleanPrimitiveType)
    an[IllegalArgumentException] should be thrownBy
      StepPointer(SelfPointer(tupleType), FeatureStep(structType, "a"))
  }

  it should "have the right composite picker" in {
    StepPointer(SelfPointer(tupleType), CoordinateStep(tupleType, 1)).picker shouldEqual
      CoordinatePicker(tupleType, 1)
  }

  def tt(i: Int): TupleType = {
    if (i == 0) TupleType(IntPrimitiveType, StringPrimitiveType)
    else TupleType(IntPrimitiveType, StringPrimitiveType, tt(i - 1))
  }

  "A Pointer" should "support concat" in {
    SelfPointer(tt(4)).concat(SelfPointer(tt(4))) shouldEqual SelfPointer(tt(4))
    val pt2 = SelfPointer(tt(4)).append(CoordinateStep(tt(4), 1))
    SelfPointer(tt(4)).concat(pt2) shouldEqual pt2
    pt2.concat(SelfPointer(StringPrimitiveType)) shouldEqual pt2
    val pt3 = SelfPointer(tt(4)).append(CoordinateStep(tt(4), 2))
    val pt4 = SelfPointer(tt(3)).append(CoordinateStep(tt(3), 2))
    val pt5 = pt3.append(CoordinateStep(tt(3), 2))
    pt3.concat(pt4) shouldEqual pt5
  }

  it should "support prepend" in {
    SelfPointer(tt(3)).prepend(CoordinateStep(tt(4), 2)) shouldEqual
      SelfPointer(tt(4)).append(CoordinateStep(tt(4), 2))
  }

  it should "validate pointer types during concat" in {
    val pt1 = SelfPointer(tt(0)).append(CoordinateStep(tt(0), 0))
    itShouldBeDisallowed calling pt1.concat(pt1)
  }

  it should "support longestCommonAncestor" in {
    val pt1 = SelfPointer(tt(4)).append(CoordinateStep(tt(4), 2)).append(CoordinateStep(tt(3), 2)).append(CoordinateStep(tt(2), 1))
    val pt2 = SelfPointer(tt(4)).append(CoordinateStep(tt(4), 2)).append(CoordinateStep(tt(3), 1))
    val pt3 = SelfPointer(tt(4)).append(CoordinateStep(tt(4), 2))
    pt1.longestCommonAncestor(pt2) shouldEqual pt3
    itShouldBeDisallowed calling pt1.longestCommonAncestor(SelfPointer(tt(3)))
  }

  it should "provide steps" in {
    val pt1 = SelfPointer(tt(4))
    pt1.steps shouldEqual List.empty
    val st1 = CoordinateStep(tt(4), 2)
    val st2 = CoordinateStep(tt(3), 1)
    val pt2 = pt1.append(st1).append(st2)
    pt2.steps shouldEqual List(st1, st2)
  }

  it should "support IntoStep-s" in {
    val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
    val seqTupleType = SeqType(tupleType)
    val strType = StructType("name", "as" -> seqTupleType)

    import Pointers._
    val ptr = (pointerTo(strType).feature("as") /@ (IntoStep(seqTupleType))).pointer
    ptr.sourceType shouldEqual strType
    ptr.targetType shouldEqual tupleType
  }
}