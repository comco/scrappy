package com.github.comco.scrappy

sealed abstract class Pointer {
  def sourceType: Type
  def targetType: Type
}

case class SelfPointer(val sourceType: Type) extends Pointer {
  def targetType = sourceType
}