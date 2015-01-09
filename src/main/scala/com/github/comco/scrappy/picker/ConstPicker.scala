package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class ConstPicker[+Target <: Shape.Any](val targetData: Data[Target])
    extends BasePicker[Shape.Any, Target] {

  override def sourceSchema = Schema.Any

  override def targetSchema = targetData.schema

  override def doPick(source: Data.Any) = targetData
}