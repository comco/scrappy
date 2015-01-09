package com.github.comco.scrappy.schema.basic

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy.Schema

case class BasicPrimitiveSchema[Raw: TypeTag](implicit val typeTag: TypeTag[Raw]) extends Schema.RichPrimitive[Raw]