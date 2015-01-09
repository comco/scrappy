package com.github.comco.scrappy.data

import com.github.comco.scrappy.Data
import com.github.comco.scrappy.Schema
import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.Shape

trait FactoryChecking extends Data.Factory {
  abstract override def struct(
    features: Map[String, Data.Any])(
      origin: Origin.Struct,
      schema: Schema.Struct) = {
    // all provided features should be valid.
    features.foreach {
      case (name, feature) =>
        require(schema.featureSchemas.contains(name),
          s"Struct schema: $schema doesn't contain feature named: $name.")

        require(feature.schema.satisfies(schema.featureSchemas(name)),
          s"The schema of the feature: $feature should satisfy: ${schema.featureSchemas(name)}.")
    }

    // all schema features should be provided.
    schema.featureSchemas.foreach {
      case (name, featureSchema) =>
        require(features.contains(name), s"Feature named: $name should be provided.")
    }

    super.struct(features)(origin, schema)
  }

  abstract override def tuple(
    coordinates: IndexedSeq[Data.Any])(
      origin: Origin.Tuple,
      schema: Schema.Tuple) = {
    // there should be the right number of coordinates.
    require(coordinates.length == schema.coordinateSchemas.length,
      s"The number of provided coordinates: ${coordinates.length} should match the number of coordinates of the schema: ${schema.coordinateSchemas.length}.")

    // all coordinates should match.
    coordinates.zipWithIndex.foreach {
      case (coordinate, position) =>
        require(coordinate.schema.satisfies(schema.coordinateSchemas(position)),
          s"The provided coordinate: $coordinate at position: $position should match the corresponsing schema: ${schema.coordinateSchemas(position)}.")
    }

    super.tuple(coordinates)(origin, schema)
  }

  abstract override def tuple[Coordinate1 <: Shape.Any](
    coordinate1: Data[Coordinate1])(
      origin: Origin.Tuple1[Coordinate1],
      schema: Schema.Tuple1[Coordinate1]) = {
    require(coordinate1.schema.satisfies(schema.coordinate1Schema),
      s"Coordinate1 data: $coordinate1 should satisfy schema: ${schema.coordinate1Schema}.")

    super.tuple(coordinate1)(origin, schema)
  }
}