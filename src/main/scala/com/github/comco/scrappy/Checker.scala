package com.github.comco.scrappy

/**
 * Represents data checks. Works reasonably both for bare data and for
 * originated data.
 */
abstract class Checker[-Source <: Shape.Any] {
  def sourceType: Type[Source]

  def checkData(source: Data[Source]): CheckResult
  def checkOriginatedData(source: OriginatedData[Source]): OriginatedCheckResult
}

case class CheckResult(val successful: Boolean)

case class OriginatedCheckResult(val successful: Boolean)