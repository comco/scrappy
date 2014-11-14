package com.github.comco.scrappy.picker

import scala.language.implicitConversions

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.repository.TypeRepository

trait PickerImplicits {
  implicit class RichPicker(val picker: Picker) {
    def andThen(next: Picker): RichPicker = {
      require(picker.targetType.isSubtypeOf(next.sourceType),
        s"The targetType of the picker: $picker must be the same as the sourceType of: $next")
      AndThenPicker(picker, next)
    }

    def feature(name: String): RichPicker = picker.targetType match {
      case targetType: StructType => picker andThen FeaturePicker(targetType, name)
      case _ => throw new IllegalArgumentException(s"Target type of picker: $picker must be a StructType.")
    }

    def feature(symbol: Symbol): RichPicker = feature(symbol.name)

    def coordinate(position: Int): RichPicker = picker.targetType match {
      case targetType: TupleType => picker andThen CoordinatePicker(targetType, position)
      case _ => throw new IllegalArgumentException(s"Target type of picker: $picker must be a TupleType.")
    }

    def element(index: Int): RichPicker = picker.targetType match {
      case targetType: SeqType => picker andThen ElementPicker(targetType, index)
      case _ => throw new IllegalArgumentException(s"Target type of picker: $picker must be a SeqType.")
    }

    def some: RichPicker = picker.targetType match {
      case targetType: OptionType => picker andThen SomePicker(targetType)
      case _ => throw new IllegalArgumentException(s"Target type of picker: $picker must be an OptionType.")
    }

    def tuple(coordinates: Picker*): RichPicker = {
      picker andThen TuplePicker(coordinates.toIndexedSeq)
    }

    def struct(structType: StructType)(features: (String, Picker)*): RichPicker = {
      picker andThen StructPicker(structType)(features: _*)
    }

    def zip(first: Picker, second: Picker): RichPicker = {
      picker andThen ZipPicker(
        first.asInstanceOf[Picker with Picker.ReturningSeq],
        second.asInstanceOf[Picker with Picker.ReturningSeq])
    }

    def struct(symbol: Symbol)(features: (Symbol, Picker)*)(implicit repo: TypeRepository): RichPicker = {
      struct(repo.getStructType(symbol))(features.map {
        case (s, p) => (s.name, p)
      }: _*)
    }

    def map(f: Picker): RichPicker = picker andThen MapPicker(f)

    def const(data: Data): RichPicker = picker andThen ConstPicker(data)
  }

  def const(data: Data): RichPicker = RichPicker(ConstPicker(data))

  implicit def Type_To_Picker(typ: Type): Picker = SelfPicker(typ)
  implicit def Type_To_RichPicker(typ: Type): RichPicker = RichPicker(SelfPicker(typ))

  implicit def Symbol_To_Picker(symbol: Symbol)(implicit repo: TypeRepository): Picker = {
    SelfPicker(repo.getNamedType(symbol))
  }

  implicit class Picker_RichType(typ: Type) {
    def pick: RichPicker = RichPicker(SelfPicker(typ))
  }

  implicit class Picker_RichSymbol(symbol: Symbol)(implicit repo: TypeRepository) {
    def pick: RichPicker = RichPicker(SelfPicker(repo.getNamedType(symbol)))
  }

  implicit def RichPicker2Picker(richPicker: RichPicker): Picker = richPicker.picker

  implicit def PrimitiveFunction_To_ApplyPicker[A, R](f: A => R)(
    implicit sourceType: PrimitiveType[A], targetType: PrimitiveType[R]) = {
    ApplyPicker[A, R](f)
  }

  def pickerTo(sourceType: Type): RichPicker = RichPicker(SelfPicker(sourceType))
}