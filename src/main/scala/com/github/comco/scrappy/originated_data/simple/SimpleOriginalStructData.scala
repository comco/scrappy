package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedStructData
import com.github.comco.scrappy.pointer.FeatureStep
import com.github.comco.scrappy.Type

case class SimpleOriginalStructData(val data: StructData, val origin: Origin)
    extends OriginatedStructData {

  lazy val features: Map[String, OriginatedData.Any] = data.features.map {
    case (name, feature) =>
      val featureOrigin = origin.append(FeatureStep(datatype, name))
      (name, OriginatedData.from(feature, featureOrigin))
  }
}