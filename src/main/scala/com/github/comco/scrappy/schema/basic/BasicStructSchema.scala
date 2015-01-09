package com.github.comco.scrappy.schema.basic

import com.github.comco.scrappy.Schema

case class BasicStructSchema(val name: String, val featureSchemas: Map[String, Schema.Any]) extends Schema.RichStruct