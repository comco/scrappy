package com.github.comco.scrappy.pointer

import scala.language.implicitConversions
import scala.language.postfixOps

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.RegexParsers

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Types

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
    
    def into: RichPointer = pointer.targetType match {
      case tt: SeqType => RichPointer(pointer.append(IntoStep(tt)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not a SeqType.")
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

  class Repository {
    def mkString(pointer: Pointer): String = pointer match {
      case SelfPointer(_) => ""
      case StepPointer(init, step) => {
        val initPart = mkString(init)
        val stepPart = step match {
          case CoordinateStep(_, position) => s"/$position"
          case FeatureStep(_, name) => s"/$name"
          case ElementStep(_, index) => s"[$index]"
          case SomeStep(_) => "$"
          case IntoStep(_) => "[*]"
        }
        return initPart + stepPart
      }
    }

    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    object Parser extends JavaTokenParsers with RegexParsers {
      def coordinate: Parser[Type => CoordinateStep] = "/" ~> wholeNumber ^^ {
        position =>
          {
            case tupleType: TupleType => CoordinateStep(tupleType, position.toInt)
            case otherType => throw new IllegalArgumentException(s"For parsing the position: $position, the type: $otherType should be a TupleType.")
          }
      }

      def feature: Parser[Type => FeatureStep] = "/" ~> ident ^^ {
        name =>
          {
            case structType: StructType => FeatureStep(structType, name)
            case otherType => throw new IllegalArgumentException(s"For parsing the name: $name, the type: $otherType should be a StructType.")
          }
      }

      def element: Parser[Type => ElementStep] = "[" ~> wholeNumber <~ "]" ^^ {
        index =>
          {
            case seqType: SeqType => ElementStep(seqType, index.toInt)
            case otherType => throw new IllegalArgumentException(s"For parsing the index: $index, the type: $otherType should be a SeqType.")
          }
      }

      def some: Parser[Type => SomeStep] = "$" ^^ {
        _ =>
          {
            case optionType: OptionType => SomeStep(optionType)
            case otherType => throw new IllegalArgumentException(s"For parsing an option (ending with ?) type, the type: $otherType should be a OptionType.")
          }
      }
      
      def into: Parser[Type => IntoStep] = "[*]" ^^ {
        _ => {
          case seqType: SeqType => IntoStep(seqType)
          case otherType => throw new IllegalArgumentException(s"For parsit an into step ([*]), the type $otherType needs to be a SeqType.")
        }
      }

      def full: Parser[Type => Pointer] = rep(coordinate | feature | element | some | into) ^^ {
        steps =>
          {
            val f: Type => Pointer = { typ => SelfPointer(typ) }
            steps.foldRight(f) {
              case (h, f) => {
                typ: Type =>
                  {
                    val head = h(typ)
                    f(head.targetType).prepend(head)
                  }
              }
            }
          }
      }
    }

    def mkPointer(sourceType: Type, string: String): Pointer = {
      Parser.parseAll(Parser.full, string).get(sourceType)
    }
    
    def mkPointer(string: String)(implicit typeRepo: Types.Repository): Pointer = {
      val typeResult = typeRepo.Parser.parse(typeRepo.Parser.full, string)
      Parser.parseAll(Parser.full, typeResult.next).get(typeResult.get)
    }
  }

  implicit object SimpleRepository extends Repository
}