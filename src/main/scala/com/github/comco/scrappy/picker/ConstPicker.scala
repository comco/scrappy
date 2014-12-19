package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

/**
 * Discards its source (leaving only the origin) and returns a constant.
 */
case class ConstPicker[+Shape <: Shape.Any](val data: Data[Shape])
    extends Picker[Shape.Any, Shape] {
  val targetType = data.datatype

  def pickData(source: Data.Any) = data

  def pickOriginatedData(source: OriginatedData.Any) =
    OriginatedData(data, source.origin)
}