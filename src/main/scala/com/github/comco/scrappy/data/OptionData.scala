package com.github.comco.scrappy.data

import com.github.comco.scrappy.OptionType

sealed abstract class OptionData extends Data.PackageSealed {
  def datatype: OptionType

  /**
   * True if this option data has some value.
   */
  def isSome: Boolean
}

object OptionData {
  private[scrappy] abstract class PackageSealed extends OptionData
}