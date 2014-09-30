package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.simple.SimpleComputedStructData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalStructData

abstract class OriginatedStructData extends OriginatedData.PackageSealed {
  def datatype: StructType = data.datatype
  def data: StructData

  /**
   * The features of this originated struct data.
   */
  def features: Map[String, OriginatedData]
  
  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(name: String): Boolean = {
    features.contains(name) && OriginatedData.isFilled(features(name))
  }

  /**
   * Retrieves the feature of this struct data with some name.
   */
  def feature(name: String): OriginatedData = {
    require(datatype.hasFeature(name),
      s"StructData doesn't contain a feature named: $name")

    features(name)
  }
}

object OriginatedStructData {
  def original(data: StructData, origin: Origin): OriginatedStructData =
    SimpleOriginalStructData(data, origin)

  def computed(data: StructData,
    origin: Origin,
    features: Map[String, OriginatedData]): OriginatedStructData =
    SimpleComputedStructData(data, origin, features)
}