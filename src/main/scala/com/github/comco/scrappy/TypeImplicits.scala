package com.github.comco.scrappy

import scala.language.implicitConversions
import scala.language.postfixOps

import com.github.comco.scrappy.repository.TypeRepository

trait TypeImplicits {
  final val int : PrimitiveType[Int] = Type.PrimitiveNil
  final val string : PrimitiveType[String] = Type.PrimitiveNil
  final val boolean : PrimitiveType[Boolean] = Type.PrimitiveNil

  def tuple(coordinateTypes: Type.Any*): TupleType = TupleType(coordinateTypes.toIndexedSeq)
  def struct(name: String, features: (String, Type.Any)*): StructType = StructType(name, features: _*)
  def seq(elementType: Type.Any): SeqType = SeqType(elementType)
  def opt(someType: Type.Any): OptionType = OptionType(someType)

  implicit def Symbol_To_Type(symbol: Symbol)(implicit repo: TypeRepository): Type.Any = {
    repo.getNamedType(symbol)
  }
}