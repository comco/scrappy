package com.github.comco.scrappy.schema.basic

import com.github.comco.scrappy.Schema

object BasicNoneSchema extends Schema.RichNone {
  final val valueSchema = BasicNothingSchema
}