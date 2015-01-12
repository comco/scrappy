package com.github.comco.scrappy

abstract class Picker[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceSchema: Schema.Any

  def targetSchema: Schema[Target]

  def pick[S <: Shape.Any](source: Data[S, Source]): Data[S, Target]
}