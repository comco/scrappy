package scrappy

abstract class Origin {
  def append(next: StepPointer): Origin = ???
}

case class Original(pointer: Pointer) extends Origin
case class Computed(pointers: Set[Pointer]) extends Origin