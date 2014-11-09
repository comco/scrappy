package com.github.comco.scrappy.picker

import scala.language.implicitConversions
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.Types

object Pickers {
  implicit class RichPicker(val picker: Picker) {
    def andThen(next: Picker): RichPicker = {
      require(picker.targetType == next.sourceType,
        s"The targetType of the picker: $picker must be the same as the sourceType of: $next")
      if (picker.isInstanceOf[SelfPicker]) {
        next
      } else if (next.isInstanceOf[SelfPicker]) {
        this
      } else {
        AndThenPicker(picker, next)
      }
    }

    def feature(name: String): RichPicker = picker.targetType match {
      case targetType: StructType => picker andThen FeaturePicker(targetType, name)
      case _ => throw new IllegalArgumentException(s"Target type of picker: $picker must be a StructType.")
    }

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

    def map(f: Picker): RichPicker = picker andThen MapPicker(f)

    def const(data: Data): RichPicker = picker andThen ConstPicker(picker.targetType, data)
  }

  object dsl {
    implicit def Symbol2RichPicker(symbol: Symbol)(implicit repo: Types.Repository): RichPicker = {
      RichPicker(SelfPicker(repo.getNamedType(symbol)))
    }
    
    implicit def Type2Picker(typ: Type): Picker = SelfPicker(typ)
    implicit def Type2RichPicker(typ: Type): RichPicker = RichPicker(Type2Picker(typ))
  }

  implicit def RichPicker2Picker(richPicker: RichPicker): Picker = richPicker.picker

  implicit def PrimitiveFunction2ApplyPicker[A, R](f: A => R)(
    implicit sourceType: PrimitiveType[A], targetType: PrimitiveType[R]) =
    ApplyPicker[A, R](f)

  def pickerTo(sourceType: Type): RichPicker = RichPicker(SelfPicker(sourceType))
}