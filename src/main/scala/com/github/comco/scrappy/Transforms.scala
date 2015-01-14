package com.github.comco.scrappy

object Transforms {
  def stripToBare(data: Data.Any)(implicit dataFactory: Data.Factory): Data.Any = data match {
    case data: Data.RichPrimitive[raw] => {
      Data.Primitive(data.raw)(data.schema.typeTag, dataFactory)
    }
    case data: Data.RichStruct => {
      val strippedFeatures = data.features.mapValues(stripToBare(_))
      Data.Struct(data.schema, strippedFeatures)
    }
    case data: Data.RichTuple1[c1] => {
      Data.Tuple(stripToBare(data.coordinate1))
    }
    case data: Data.RichTuple2[c1, c2] => {
      Data.Tuple(stripToBare(data.coordinate1), stripToBare(data.coordinate2))
    }
    case data: Data.RichTuple => {
      Data.Tuple(data.coordinates.map(stripToBare(_)))
    }
    case data: Data.RichSequence[e] => {
      Data.Sequence(data.elements.map(stripToBare(_)))
    }
    case data: Data.RichSome[v] => {
      Data.Some(stripToBare(data.value).asInstanceOf[Data.Concrete])
    }
    case data: Data.RichNone => {
      Data.None()
    }
  }
}