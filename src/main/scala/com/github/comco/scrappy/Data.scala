package com.github.comco.scrappy

import scala.reflect.runtime.universe._

sealed abstract class Data[+Shape <: Shape.Any] extends Data.Mixin[Shape]

object Data extends DataDomain {
  type Abstract[+Shape <: Shape.Any] = Data[Shape]

  trait Factory {
    def Primitive[RawType: TypeTag](raw: RawType): Primitive[RawType]

    def Struct(datatype: Type.Struct, features: Map[String, Any]): Struct
  }

  def Any = ???
  def Nil = ???

  implicit val DefaultFactory: Factory = DefaultData
}