package com.github.comco.scrappy.pointer.dsl

import scala.language.implicitConversions
import scala.language.postfixOps
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.pointer.FeatureStep
import com.github.comco.scrappy.pointer.Pointer
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.Step
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.StepPointer
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.SomeStep
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.pointer.SomeStep
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.pointer.SomeStep

object Pointers {
  implicit class RichPointer(val pointer: Pointer) {
    /**
     * Appends a FeatureStep to this pointer if its target is a StructType.
     */
    def feature(name: String): RichPointer = pointer.targetType match {
      case tt: StructType => RichPointer(pointer.append(FeatureStep(tt, name)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not a StructType.")
    }

    /**
     * Appends a CoordinateStep to this pointer if its target is a TupleType.
     */
    def coordinate(position: Int): RichPointer = pointer.targetType match {
      case tt: TupleType => RichPointer(pointer.append(CoordinateStep(tt, position)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not a TupleType.")
    }

    /**
     * Appends an ElementStep to this pointer it its target is a SeqType.
     */
    def element(index: Int): RichPointer = pointer.targetType match {
      case tt: SeqType => RichPointer(pointer.append(ElementStep(tt, index)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not a SeqType.")
    }

    def some: RichPointer = pointer.targetType match {
      case tt: OptionType => RichPointer(pointer.append(SomeStep(tt)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not an OptionType.")
    }

    /**
     * Appends a Step to the pointer.
     */
    def /@(step: Step): RichPointer = RichPointer(pointer.append(step))

    /**
     * Appends a sequence of Steps to the pointer.
     */
    def apply(steps: Step*): RichPointer = {
      if (steps.isEmpty) this
      else (this /@ steps.head)(steps.tail: _*)
    }

  }

  def pointerTo(rootType: Type) = RichPointer(SelfPointer(rootType))
  implicit def RichPointer2Pointer(rich: RichPointer): Pointer = rich.pointer

  implicit class RichStructType(structType: StructType) {
    def $(name: String) = FeatureStep(structType, name)
  }

  implicit class RichTupleType(tupleType: TupleType) {
    def $(position: Int) = CoordinateStep(tupleType, position)
  }

  implicit class RichSeqType(seqType: SeqType) {
    def $(index: Int) = ElementStep(seqType, index)
  }

  implicit class RichOptionType(optionType: OptionType) {
    def $ = SomeStep(optionType)
  }

  implicit class RichType(typ: Type) {
    def $(part: Any) = (typ, part) match {
      case (typ: StructType, name: String) => RichStructType(typ) $ name
      case (typ: TupleType, position: Int) => RichTupleType(typ) $ position
      case (typ: SeqType, index: Int) => RichSeqType(typ) $ index
      case _ => throw new IllegalArgumentException(s"Cannot construct pointer step from type: $typ and part: $part.")
    }

    def $ = typ match {
      case typ: OptionType => RichOptionType(typ) $
      case _ => throw new IllegalArgumentException(s"Cannot construct option pointer step from type: $typ.")
    }
  }

  class StringConvertor {
    def mkString(pointer: Pointer): String = pointer match {
      case SelfPointer(_) => ""
      case StepPointer(init, step) => {
        val initPart = mkString(init)
        val stepPart = step match {
          case CoordinateStep(_, position) => s"/$position"
          case FeatureStep(_, name) => s"/$name"
          case ElementStep(_, index) => s"[$index]"
          case SomeStep(_) => "$"
        }
        return initPart + stepPart
      }
    }

    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    def mkPointer(sourceType: Type, string: String): Option[Pointer] = (sourceType, string) match {
      case (sourceType, "") => Some(SelfPointer(sourceType))

      case (sourceType: TupleType, r"/(\d+)$positionText(.*)$rest") => {
        val position = positionText.toInt
        for {
          restPointer <- mkPointer(sourceType.coordinateType(position), rest)
        } yield restPointer.prepend(sourceType $ position)
      }

      case (sourceType: StructType, r"/(\w+)$name(.*)$rest") => for {
        restPointer <- mkPointer(sourceType.featureType(name), rest)
      } yield restPointer.prepend(sourceType $ name)

      case (sourceType: SeqType, r"\[(\d+)$indexText\](.*)$rest") => {
        val index = indexText.toInt
        for {
          restPointer <- mkPointer(sourceType.elementType, rest)
        } yield restPointer.prepend(sourceType $ index)
      }

      case (sourceType: OptionType, r"$$(.*)$rest") => for {
        restPointer <- mkPointer(sourceType.someType, rest)
      } yield restPointer.prepend(sourceType $)

      case _ => None
    }
  }

  implicit object DefaultStringConvertor extends StringConvertor
}