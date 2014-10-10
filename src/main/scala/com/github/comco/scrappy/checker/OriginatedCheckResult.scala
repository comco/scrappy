package com.github.comco.scrappy.checker

import com.github.comco.scrappy.origin.Origin

case class OriginatedCheckResult(val successful: Boolean, val origin: Origin)