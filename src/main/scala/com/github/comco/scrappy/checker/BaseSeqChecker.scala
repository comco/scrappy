package com.github.comco.scrappy.checker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

abstract class BaseSeqChecker extends BaseChecker {
  def doCheckData(source: Data): CheckResult = {
    doCheckData(source.asInstanceOf[SeqData])
  }
  
  def doCheckData(source: SeqData): CheckResult
  
  def doCheckOriginatedData(source: OriginatedData): OriginatedCheckResult = {
    doCheckOriginatedData(source.asInstanceOf[OriginatedSeqData])
  }
  
  def doCheckOriginatedData(source: OriginatedSeqData): OriginatedCheckResult
}