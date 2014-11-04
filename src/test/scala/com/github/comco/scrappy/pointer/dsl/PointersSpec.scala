package com.github.comco.scrappy.pointer.dsl

import scala.language.postfixOps
import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.FeatureStep
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.StepPointer
import Pointers.RichPointer2Pointer
import Pointers.RichSeqType
import Pointers.RichStructType
import Pointers.RichTupleType
import Pointers.RichType
import Pointers.pointerTo
import com.github.comco.scrappy.pointer.dsl.Pointers.DefaultStringConvertor
import com.github.comco.scrappy.pointer.SomeStep
import com.github.comco.scrappy.OptionType
import scala.util.Success
import scala.util.Failure

final class PointersSpec extends FlatSpec with CustomMatchers {
  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val structType = StructType("str", "a" -> tupleType, "b" -> BooleanPrimitiveType)
  val seqType = SeqType(structType)
  val optionType = OptionType(IntPrimitiveType)

  "Pointers" should "provide implicit RichPointer construction & deconstruction" in {
    pointerTo(tupleType).sourceType shouldEqual tupleType
    pointerTo(tupleType).coordinate(0).pointer shouldEqual StepPointer(SelfPointer(tupleType), CoordinateStep(tupleType, 0))
    pointerTo(seqType).element(3).feature("a").coordinate(1).pointer shouldEqual
      StepPointer(StepPointer(StepPointer(SelfPointer(seqType), ElementStep(seqType, 3)), FeatureStep(structType, "a")), CoordinateStep(tupleType, 1))
    pointerTo(optionType).some.pointer shouldEqual
      StepPointer(SelfPointer(optionType), SomeStep(optionType))
  }

  it should "check RichPointer feature type" in {
    itShouldBeDisallowed calling pointerTo(tupleType).feature("b")
    itShouldBeDisallowed calling pointerTo(tupleType).element(3)
    itShouldBeDisallowed calling pointerTo(structType).coordinate(4)
    itShouldBeDisallowed calling pointerTo(structType).some
  }

  it should "provide Typed Steps construction" in {
    // Strongly-typed
    (tupleType $ 0) shouldEqual CoordinateStep(tupleType, 0)
    (structType $ "b") shouldEqual FeatureStep(structType, "b")
    (seqType $ 3) shouldEqual ElementStep(seqType, 3)
    (optionType $) shouldEqual SomeStep(optionType)

    // Weakly-typed
    ((tupleType: Type) $ 0) shouldEqual CoordinateStep(tupleType, 0)
    ((structType: Type) $ "b") shouldEqual FeatureStep(structType, "b")
    ((seqType: Type) $ 3) shouldEqual ElementStep(seqType, 3)
  }

  it should "check argument types in weakly-typed Steps construction" in {
    itShouldBeDisallowed calling ((tupleType: Type) $ "hi")
    itShouldBeDisallowed calling ((structType: Type) $ 3)
    itShouldBeDisallowed calling ((seqType: Type) $ false)
    itShouldBeDisallowed calling ((seqType: Type) $)
  }

  it should "support appending steps directly" in {
    (pointerTo(seqType) /@ (seqType $ 0) /@ (structType $ "a") /@ (tupleType $ 1)).pointer shouldEqual
      StepPointer(StepPointer(StepPointer(SelfPointer(seqType), ElementStep(seqType, 0)), FeatureStep(structType, "a")), CoordinateStep(tupleType, 1))
  }

  it should "support appending a sequence of Steps at once" in {
    pointerTo(seqType)(seqType $ 0, structType $ "a").pointer shouldEqual
      StepPointer(StepPointer(SelfPointer(seqType), ElementStep(seqType, 0)), FeatureStep(structType, "a"))
  }

  "A Pointers.DefaultStringConvertor" should "support mkString, mkPointer" in {
    val pointer = (pointerTo(seqType) /@ (seqType $ 0) /@ (structType $ "a") /@ (tupleType $ 1)).pointer
    val optionPointer = (pointerTo(optionType) /@ (optionType $)).pointer
    
    DefaultStringConvertor.mkString(pointer) shouldEqual "[0]/a/1"
    DefaultStringConvertor.mkString(optionPointer) shouldEqual "$"
    
    DefaultStringConvertor.mkPointer(seqType, "") shouldEqual Success(SelfPointer(seqType))
    DefaultStringConvertor.mkPointer(seqType, "[0]") shouldEqual Success(StepPointer(SelfPointer(seqType), ElementStep(seqType, 0)))
    DefaultStringConvertor.mkPointer(seqType, "[0]/a/1") shouldEqual Success(pointer)
    DefaultStringConvertor.mkPointer(optionType, "$") shouldEqual Success(optionPointer)
  }

  it should "check for wrong pointers" in {
    DefaultStringConvertor.mkPointer(seqType, "/") shouldBe a[Failure[_]]
    DefaultStringConvertor.mkPointer(structType, "//") shouldBe a[Failure[_]]
  }
}