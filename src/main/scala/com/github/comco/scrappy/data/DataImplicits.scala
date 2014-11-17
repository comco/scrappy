package com.github.comco.scrappy.data

import scala.language.implicitConversions
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.TypeImplicits
import com.github.comco.scrappy.repository.TypeRepository
import com.github.comco.scrappy.Type

trait DataImplicits {
  def tuple(coordinates: Data[Type[Any]]*): TupleData = {
    TupleData(coordinates: _*)
  }

  def seq(elements: Data[Type[Any]]*): SeqData = {
    SeqData(elements.head, elements.tail: _*)
  }

  def struct(symbol: Symbol)(features: (Symbol, Data[Type[Any]])*)(implicit repo: TypeRepository): StructData = {
    StructData(repo.getStructType(symbol))(features.map {
      case (sym, data) => (sym.name, data)
    }: _*)
  }

  def some(data: Data[Type[Any]]): SomeData = {
    val someType = OptionType(data.datatype)
    SomeData(someType, data)
  }

  implicit def Primitive_To_PrimitiveData[T](value: T)(
    implicit typ: PrimitiveType[T]) = PrimitiveData(value)

  implicit class RichSymbol(symbol: Symbol) {
    def of(features: (Symbol, Data[Type[Any]])*)(implicit repo: TypeRepository): StructData =
      struct(symbol)(features: _*)
  }

  implicit class RichData(data: Data[Type[Any]]) {
    def element(index: Int): Data[Type[Any]] = data match {
      case data: SeqData => data.element(index)
      case _ => throw new IllegalArgumentException(s"Data: $data is not a SeqData.")
    }

    def feature(name: Symbol): Data[Type[Any]] = data match {
      case data: StructData => data.feature(name.name)
      case _ => throw new IllegalArgumentException(s"Data: $data is not a StructData.")
    }

    def coordinate(position: Int): Data[Type[Any]] = data match {
      case data: TupleData => data.coordinate(position)
      case _ => throw new IllegalArgumentException(s"Data: $data is not a TupleData.")
    }

    def some: Data[Type[Any]] = data match {
      case data: SomeData => data.value
      case _ => throw new IllegalArgumentException(s"Data: $data is not a SomeData.")
    }
  }
}