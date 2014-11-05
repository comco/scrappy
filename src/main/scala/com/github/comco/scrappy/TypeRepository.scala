package com.github.comco.scrappy

import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * A type repository represents an immutable collection of named types.
 * It provides services like: registering a new type to the repository,
 * conflict detection and resolution and textual representation-to-actual types
 * convertion.
 */
abstract class TypeRepository {
  /**
   * Adds a type and transitively adds all of it parts.
   * Throws a TypeConflictException when a type with the same name has already been registered.
   */
  def addType(typ: Type): TypeRepository

  /**
   * Creates a string representation of a type.
   */
  def mkString(typ: Type): String = typ match {
    case typ: PrimitiveType[t] => typ.typeName
    case TupleType(coordinateTypes) => coordinateTypes.map(mkString(_)).mkString("(", ", ", ")")
    case StructType(name, _) => name
    case SeqType(elementType) => s"[${mkString(elementType)}]"
    case OptionType(someType) => s"${mkString(someType)}?"
  }

  /**
   * Constructs a type from its textual representation.
   * Throws a TypeMissingException when a type cannot be found in the repository.
   */
  def getType(text: String): Type = Parser.parse(Parser.full, text).get

  /**
   * Returns a type of the given name.
   */
  def getNamedType(name: String): Type

  /**
   * Parser for textual representation of types.
   */
  object Parser extends JavaTokenParsers {
    def named: Parser[Type] = ident ^^ (getNamedType(_))
    def tuple: Parser[TupleType] = ("(" ~> repsep(full, ",") <~ ")") ^^ {
      case types => TupleType(types.toIndexedSeq)
    }
    def seq: Parser[SeqType] = ("[" ~> full <~ "]") ^^ (SeqType(_))
    def full: Parser[Type] = ((named | tuple | seq) ~ opt("?")) ^^ {
      case p ~ None => p
      case p ~ Some(_) => OptionType(p)
    }
  }
}

class TypeConflictException(message: String) extends IllegalStateException(message)
class TypeMissingException(message: String) extends IllegalArgumentException(message)

object TypeRepository {
  case class SimpleTypeRepository(typeRegistry: Map[String, Type]) extends TypeRepository {
    def addType(typ: Type): SimpleTypeRepository = typ match {
      case typ: PrimitiveType[_] => this
      case StructType(name, featureTypes) => {
        if (typeRegistry.contains(name)) {
          typeRegistry.get(name) match {
            case Some(inTyp) =>
              if (inTyp != typ) {
                throw new TypeConflictException(s"A type with name: $name already exists in the repository as: $inTyp which is different from the requested: $typ.")
              }
            case _ => // nothing
          }
        }
        val repo = SimpleTypeRepository(typeRegistry + (name -> typ))
        return (repo /: featureTypes.values) {
          (repo, typ) => repo.addType(typ)
        }
      }
      case TupleType(coordinateTypes) => (this /: coordinateTypes) {
        (repo, typ) => repo.addType(typ)
      }
      case OptionType(someType) => addType(someType)
      case SeqType(elementType) => addType(elementType)
    }

    def getNamedType(name: String) =
      PrimitiveType.typeNames.getOrElse(name,
        typeRegistry.getOrElse(name,
          throw new TypeMissingException(s"Can't resolve type name: $name in this type repository.")))
  }

  final val empty = SimpleTypeRepository(Map.empty)
}