package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class SelfPicker[Shape <: Shape.Any](val sourceSchema: Schema[Shape])
    extends BasePicker[Shape, Shape] {
    
  override def targetSchema = sourceSchema
  
  override def doPick(source: Data[Shape]) = source
}