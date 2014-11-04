package com.github.comco.scrappy

import scala.language.postfixOps
import scala.collection.mutable.{ Map => MutableMap }
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import scala.util.parsing.combinator.JavaTokenParsers

abstract class TypeRepository {
  /**
   * Registers a user-defined struct type with this repository.
   */
  def registerStructType(structType: StructType)

  /**
   * Unregisters a user-defined struct type with this repository.
   */
  def unregisterStructType(name: String)

  /**
   * Creates a string representation of a type.
   */
  def mkString(typ: Type): String

  /**
   * Returns a type from its textual representation.
   */
  def getType(text: String): Type
}

object TypeRepository {
  class Default extends TypeRepository {
    val registeredStructTypes: MutableMap[String, StructType] = MutableMap.empty

    def registerStructType(structType: StructType) {
      registeredStructTypes.update(structType.name, structType)
    }

    def unregisterStructType(name: String) {
      registeredStructTypes.remove(name)
    }

    def mkString(typ: Type): String = typ match {
      case typ: PrimitiveType[t] => typ.typeName
      case TupleType(coordinateTypes) => coordinateTypes.map(mkString(_)).mkString("(", ", ", ")")
      case StructType(name, _) => name
      case SeqType(elementType) => s"[${mkString(elementType)}]"
      case OptionType(someType) => s"${mkString(someType)}?"
    }

    object Parser extends JavaTokenParsers {
      def named: Parser[Type] = ident ^^ {
        name => PrimitiveType.typeNames.getOrElse(name, registeredStructTypes(name))
      }
      def tuple: Parser[TupleType] = ("(" ~> repsep(full, ",") <~ ")") ^^ {
        case types => TupleType(types.toIndexedSeq)
      }
      def seq: Parser[SeqType] = ("[" ~> full <~ "]") ^^ (SeqType(_))
      def full: Parser[Type] = ((named | tuple | seq) ~ opt("?")) ^^ {
        case p~None => p
        case p~Some(_) => OptionType(p)
      }
    }

    def getType(text: String): Type = Parser.parse(Parser.full, text).get
  }
}