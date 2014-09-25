package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.StructData

abstract class OriginatedStructData extends OriginatedData.Base {
  def datatype: StructType = data.datatype
  def data: StructData
  
  def features: Map[String, OriginatedData]
}