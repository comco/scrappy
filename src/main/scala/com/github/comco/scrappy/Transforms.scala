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

  def toOriginal(data: Data.Any, origin: Origin.Any)(
      implicit dataFactory: Data.Factory): Data.Any = data match {
    case data: Data.RichPrimitive[r] => {
      val typedOrigin = origin.asInstanceOf[Origin.Primitive[r]]
      Data.Primitive(data.schema, typedOrigin, data.raw)(data.schema.typeTag, dataFactory)
    }
    case data: Data.RichStruct => {
      val typedOrigin = origin.asInstanceOf[Origin.Struct]
      val modifiedFeatures = data.features.map {
        case (name, feature) => (name, toOriginal(feature, typedOrigin.append(FeatureStep(data.schema, name))))
      }
      Data.Struct(data.schema, typedOrigin, modifiedFeatures)
    }
    case data: Data.RichTuple1[c1] => {
      val typedOrigin = origin.asInstanceOf[Origin.Tuple1[c1]]
      val modifiedCoordinate1 = toOriginal(data.coordinate1, typedOrigin.append(Coordinate11Step(data.schema)))
      Data.Tuple(data.schema, typedOrigin, modifiedCoordinate1)
    }
    case data: Data.RichTuple2[c1, c2] => {
      val typedOrigin = origin.asInstanceOf[Origin.Tuple2[c1, c2]]
      val modifiedCoordinate1 = toOriginal(data.coordinate1, typedOrigin.append(Coordinate21Step(data.schema)))
      val modifiedCoordinate2 = toOriginal(data.coordinate2, typedOrigin.append(Coordinate22Step(data.schema)))
      Data.Tuple(data.schema, typedOrigin, modifiedCoordinate1, modifiedCoordinate2)
    }
    case data: Data.RichTuple => {
      val typedOrigin = origin.asInstanceOf[Origin.Tuple]
      val modifiedCoordinates = data.coordinates.zipWithIndex.map {
        case (coordinate, position) => toOriginal(coordinate, typedOrigin.append(CoordinateStep(data.schema, position)))
      }
      Data.Tuple(data.schema, typedOrigin, modifiedCoordinates)
    }
    case data: Data.RichSequence[e] => {
      val typedOrigin = origin.asInstanceOf[Origin.Sequence[e]]
      val modifiedElements = data.elements.zipWithIndex.map {
        case (element, index) => toOriginal(element, typedOrigin.append(ElementStep(data.schema, index)))
      }
      Data.Sequence(data.schema, typedOrigin, modifiedElements)
    }
    case data: Data.RichSome[v] => {
      val typedOrigin = origin.asInstanceOf[Origin.Optional[v]]
      val modifiedValue = toOriginal(data.value, typedOrigin.append(ValueStep(data.schema))).asInstanceOf[Data[v]]
      Data.Some(data.schema, typedOrigin, modifiedValue)
    }
    case data: Data.RichNone => {
      Data.None(origin.asInstanceOf[Origin.None])
    }
  }
}