package com.github.comco.scrappy

import scala.language.implicitConversions
import scala.language.postfixOps

import scala.util.parsing.combinator.JavaTokenParsers

object Types {
  /**
   * A type repository represents an immutable collection of named types.
   * It provides services like: registering a new type to the repository,
   * conflict detection and resolution and textual representation-to-actual types
   * conversion.
   */
  abstract class Repository {
    /**
     * Adds a type and transitively adds all of it parts.
     * Throws a TypeConflictException when a type with the same name has already been registered.
     */
    def addType(typ: Type): Repository

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

  object Repository {
    case class Simple(typeRegistry: Map[String, Type]) extends Repository {
      def addType(typ: Type): Simple = typ match {
        case typ: PrimitiveType[_] => this
        case StructType(name, featureTypes) => {
          if (typeRegistry.contains(name)) {
            typeRegistry.get(name) match {
              case Some(inTyp) =>
                if (inTyp != typ) {
                  throw new TypeConflictException(s"A type with name: $name already exists in the repository as: $inTyp which is different from the requested: $typ.")
                }
              case _ => // do nothing
            }
          }
          val repo = Simple(typeRegistry + (name -> typ))
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

    final val empty = Simple(Map.empty)

    /**
     * Provides a domain-specific language for easily defining new struct types
     * based on an existing type repository.
     * WARNING: This is a class meant as a utility, don't play funny games with it!
     */
    abstract class Extension(private[this] var _repo: Repository = empty) extends Repository {
      implicit def repository: Repository = _repo

      def addType(typ: Type) = repository.addType(typ)

      def getNamedType(name: String) = repository.getNamedType(name)

      import PrimitiveType._
      
      protected def tuple(coordinateTypes: Type*): TupleType = TupleType(coordinateTypes.toIndexedSeq)
      protected def struct(name: String, features: (String, Type)*): StructType = StructType(name, features: _*)
      protected def seq(elementType: Type): SeqType = SeqType(elementType)
      protected def opt(someType: Type): OptionType = OptionType(someType)
      
      protected val int = IntPrimitiveType
      protected val string = StringPrimitiveType
      protected val boolean = BooleanPrimitiveType

      protected implicit class RichSymbol(symbol: Symbol) {
        def is(features: (Symbol, Type)*): StructType = {
          val s = struct(symbol.name, features.map {
            case (s, t) => (s.name, t)
          }: _*)

          _repo = _repo.addType(s)
          s
        }
      }

      protected implicit def Tuple2_To_TupleType(coordinateTypes: (Type, Type)): TupleType = tuple(coordinateTypes._1, coordinateTypes._2)

      protected implicit def Tuple3_To_TupleType(coordinateTypes: (Type, Type, Type)): TupleType = tuple(coordinateTypes._1, coordinateTypes._2, coordinateTypes._3)

      protected implicit def Symbol_To_Type(symbol: Symbol)(implicit repo: Types.Repository): Type = {
        repo.getNamedType(symbol.name)
      }
    }
  }
}