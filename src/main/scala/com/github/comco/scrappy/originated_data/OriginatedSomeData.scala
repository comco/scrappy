package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.data.SomeData

abstract class OriginatedSomeData extends OriginatedOptionData.Base {
  def datatype = data.datatype
  def data: SomeData
  
  def value: OriginatedData
}