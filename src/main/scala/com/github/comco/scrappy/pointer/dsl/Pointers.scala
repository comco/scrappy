package com.github.comco.scrappy.pointer.dsl

import scala.language.implicitConversions
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.pointer.Pointer
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.pointer.FeatureStep
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.pointer.Step
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.pointer.CoordinateStep
import com.github.comco.scrappy.pointer.ElementStep

object Pointers {
  implicit class RichPointer(val pointer: Pointer) {
    /**
     * Appends a FeatureStep to this pointer if its target is a StructType.
     */
    def feature(name: String): RichPointer = pointer.targetType match {
      case tt: StructType => RichPointer(pointer.append(FeatureStep(tt, name)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not StructType.")
    }

    /**
     * Appends a CoordinateStep to this pointer if its target is a TupleType.
     */
    def coordinate(position: Int): RichPointer = pointer.targetType match {
      case tt: TupleType => RichPointer(pointer.append(CoordinateStep(tt, position)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not TupleType.")
    }

    /**
     * Appends an ElementStep to this pointer it its target is a SeqType.
     */
    def element(index: Int): RichPointer = pointer.targetType match {
      case tt: SeqType => RichPointer(pointer.append(ElementStep(tt, index)))
      case _ => throw new IllegalArgumentException(s"${pointer.targetType} is not SeqType.")
    }

    /**
     * Appends a Step to the pointer.
     */
    def /@(step: Step): RichPointer = RichPointer(pointer.append(step))
    
    // TODO: Add Apply with a sequence of Step-s
    
  }

  def pointerTo(rootType: Type) = RichPointer(SelfPointer(rootType))
  implicit def RichPointer2Pointer(rich: RichPointer): Pointer = rich.pointer

  implicit class RichStructType(structType: StructType) {
    def $(name: String) = FeatureStep(structType, name)
  }
  
  implicit class RichTupleType(tupleType: TupleType) {
    def $(position: Int) = CoordinateStep(tupleType, position)
  }
  
  implicit class RichSeqType(seqType: SeqType) {
    def $(index: Int) = ElementStep(seqType, index)
  }
  
  implicit class RichType(typ: Type) {
    def $(part: Any) = (typ, part) match {
      case (typ: StructType, name: String) => RichStructType(typ) $ name
      case (typ: TupleType, position: Int) => RichTupleType(typ) $ position
      case (typ: SeqType, index: Int) => RichSeqType(typ) $ index
      case _ => throw new IllegalArgumentException(s"Cannot construct pointer step from type: $typ and part: $part.")
    }
  }
}