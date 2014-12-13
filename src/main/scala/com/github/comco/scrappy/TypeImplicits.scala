package com.github.comco.scrappy

import scala.language.implicitConversions
import scala.language.postfixOps

import com.github.comco.scrappy.PrimitiveType.BooleanPrimitiveType
import com.github.comco.scrappy.PrimitiveType.IntPrimitiveType
import com.github.comco.scrappy.PrimitiveType.StringPrimitiveType
import com.github.comco.scrappy.repository.TypeRepository

trait TypeImplicits {
  final val int = IntPrimitiveType
  final val string = StringPrimitiveType
  final val boolean = BooleanPrimitiveType

  def tuple(coordinateTypes: Type*): TupleType = TupleType(coordinateTypes.toIndexedSeq)
  def struct(name: String, features: (String, Type)*): StructType = StructType(name, features: _*)
  def seq(elementType: Type): SeqType = SeqType(elementType)
  def opt(someType: Type): OptionType = OptionType(someType)

  implicit def Symbol_To_Type(symbol: Symbol)(implicit repo: TypeRepository): Type = {
    repo.getNamedType(symbol)
  }
}