package com.github.comco.scrappy.data.simple

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SomeData

case class SimpleSomeData(val datatype: OptionType, val value: Data)
  extends SomeData