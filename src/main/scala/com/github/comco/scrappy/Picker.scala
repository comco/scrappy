package com.github.comco.scrappy

/**
 * Represents data transformations. A picker instance works both on bare data
 * and on originated data.
 */
abstract class Picker[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceType: Type[Source]
  def targetType: Type[Target]

  def pickData(source: Data[Source]): Data[Target]
  def pickOriginatedData(source: OriginatedData[Source]): OriginatedData[Target]
}