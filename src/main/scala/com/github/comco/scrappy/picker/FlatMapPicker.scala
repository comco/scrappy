package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * A FlatMapPicker: (a -> [b]) -> [a] -> [b]
 */
case class FlatMapPicker[-A <: Shape.Any: TypeTag, +B <: Shape.Any: TypeTag](val f: Picker[A, Shape.Seq[B]])
    extends BasePicker[Shape.Seq[A], Shape.Seq[B]] {
  def sourceType = SeqType(f.sourceType)
  def targetType = f.targetType

  def doPickData(source: Data.Seq[A]): Data.Seq[B] = {
    ???
  }

  def doPickOriginatedData(source: OriginatedData.Seq[A]): OriginatedData.Seq[B] = {
    ???
  }
}