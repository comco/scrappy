package com.github.comco.scrappy.pointer

import scala.language.postfixOps
import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.AndThenPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.MapPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.repository.TypeRepository
import com.github.comco.scrappy.repository.TypeMissingException

final class PointerImplicitsSpec extends FlatSpec with CustomMatchers {
  object TestPointerImplicits extends PointerImplicits
  import TestPointerImplicits._

  val tupleType = TupleType(IntPrimitiveType, StringPrimitiveType)
  val structType = StructType("str", "a" -> tupleType, "b" -> BooleanPrimitiveType)
  val seqType = SeqType(structType)
  val optionType = OptionType(IntPrimitiveType)

  "Pointers" should "provide implicit RichPointer construction & deconstruction" in {
    pointerTo(tupleType).sourceType shouldEqual tupleType
    pointerTo(tupleType).coordinate(0).pointer shouldEqual StepPointer(SelfPointer(tupleType), CoordinateStep(tupleType, 0))
    seqType.to.element(3).feature("a").coordinate(1).pointer shouldEqual
      StepPointer(StepPointer(StepPointer(SelfPointer(seqType), ElementStep(seqType, 3)), FeatureStep(structType, "a")), CoordinateStep(tupleType, 1))
    optionType.to.some.pointer shouldEqual
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

  val pointer = (pointerTo(seqType) /@ (seqType $ 0) /@ (structType $ "a") /@ (tupleType $ 1)).pointer
  val optionPointer = (pointerTo(optionType) /@ (optionType $)).pointer

  "A Pointers.DefaultStringConvertor" should "support mkString" in {
    mkString(pointer) shouldEqual "[0]/a/1"
    mkString(optionPointer) shouldEqual "$"
  }

  it should "support mkPointer" in {
    mkPointer(seqType, "") shouldEqual SelfPointer(seqType)
    mkPointer(seqType, "[0]") shouldEqual StepPointer(SelfPointer(seqType), ElementStep(seqType, 0))
    mkPointer(seqType, "[0]/a/1") shouldEqual pointer
    mkPointer(optionType, "$") shouldEqual optionPointer
    mkPointer(seqType, "[*]") shouldEqual StepPointer(SelfPointer(seqType), IntoStep(seqType))
  }

  it should "check for wrong pointers" in {
    a[RuntimeException] should be thrownBy mkPointer(seqType, "/")
    a[RuntimeException] should be thrownBy mkPointer(structType, "//")
  }

  it should "support implicit construction" in {
    implicit val repo = TypeRepository.empty.addType(seqType)
    mkPointer("str") shouldEqual mkPointer(structType, "")
    mkPointer("[str][0]/a/1") shouldEqual mkPointer(seqType, "[0]/a/1")
    mkPointer("[str][*]/a") shouldEqual mkPointer(seqType, "[*]/a")
    a[TypeMissingException] should be thrownBy mkPointer("missin.")
    a[RuntimeException] should be thrownBy mkPointer("str.[*]")
    mkPointer("[str][*]/a/1") shouldEqual StepPointer(StepPointer(StepPointer(SelfPointer(seqType), IntoStep(seqType)), FeatureStep(structType, "a")), CoordinateStep(tupleType, 1))
  }

  it should "have correct pickers" in {
    val line = StructType("line", "number" -> IntPrimitiveType, "content" -> StringPrimitiveType)
    val page = StructType("page", "lines" -> SeqType(line))
    val document = StructType("document", "pages" -> SeqType(page))

    implicit val repo = TypeRepository.empty.addType(seqType).addType(document)

    mkPointer("[str]").picker shouldEqual SelfPicker(seqType)
    mkPointer("[str][0]").picker shouldEqual ElementPicker(seqType, 0)
    mkPointer("[str][*]").picker shouldEqual SelfPicker(seqType)
    mkPointer("[str][0]/a").picker shouldEqual
      AndThenPicker(ElementPicker(seqType, 0), FeaturePicker(structType, "a"))

    mkPointer("[str][*]/a").picker shouldEqual
      MapPicker(FeaturePicker(structType, "a"))

    mkPointer("[str][*]/a/1").picker shouldEqual
      MapPicker(AndThenPicker(FeaturePicker(structType, "a"), CoordinatePicker(tupleType, 1)))

    mkPointer("document/pages[*]").picker shouldEqual
      FeaturePicker(document, "pages")

    mkPointer("document/pages[*]/lines").picker shouldEqual
      AndThenPicker(FeaturePicker(document, "pages"), MapPicker(FeaturePicker(page, "lines")))

    mkPointer("document/pages[*]/lines[*]").picker shouldEqual
      AndThenPicker(FeaturePicker(document, "pages"), MapPicker(FeaturePicker(page, "lines")))

    mkPointer("document/pages[*]/lines[*]/number").picker shouldEqual
      AndThenPicker(FeaturePicker(document, "pages"), MapPicker(
        AndThenPicker(FeaturePicker(page, "lines"), MapPicker(FeaturePicker(line, "number")))))
  }
}