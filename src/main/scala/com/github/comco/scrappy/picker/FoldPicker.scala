package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Lifts a raw fold function to a value.
 * fold([ra] -> rb) :: [a] -> b.
 */
case class FoldPicker[-RawArgumentType: TypeTag, +RawResultType: TypeTag](f: Seq[RawArgumentType] => RawResultType)(
  implicit sourceElementType: PrimitiveType[RawArgumentType],
  val targetType: PrimitiveType[RawResultType])
    extends BasePicker[Shape.Seq[Shape.Primitive[RawArgumentType]], Shape.Primitive[RawResultType]] {
  type SourceShape = Shape.Seq[Shape.Primitive[RawArgumentType]]
  type TargetShape = Shape.Primitive[RawResultType]

  def sourceType = SeqType(sourceElementType)

  def doPickData(source: Data[SourceShape]): PrimitiveData[RawResultType] = {
    PrimitiveData(f(source.elements.map(_.raw)))
  }

  def doPickOriginatedData(source: OriginatedData[SourceShape]): OriginatedPrimitiveData[RawResultType] = {
    val pickedData = doPickData(source.data)
    OriginatedPrimitiveData(pickedData,
      source.origin.computedWithTargetType(pickedData.datatype))
  }
}