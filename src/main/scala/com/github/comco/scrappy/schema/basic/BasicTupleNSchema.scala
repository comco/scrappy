package com.github.comco.scrappy.schema.basic

import com.github.comco.scrappy.Schema

case class BasicTupleNSchema(val coordinateSchemas: IndexedSeq[Schema.Any]) extends Schema.RichTuple