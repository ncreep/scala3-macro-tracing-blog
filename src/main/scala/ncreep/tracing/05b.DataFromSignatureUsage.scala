package ncreep.tracing

class DataFromSignatureUsage:
  import DataFromSignature.*

  private given tracer: Tracer = Tracer.make[DataFromSignatureUsage]

  // here we have both type and argument lists
  // we also have all the things that we shouldn't trace (by-names, `UntraceableData` and a given argument)
  // the resulting generated code traces all the valid arguments and the enclosing method's name without us
  // needing to specify any of those
  def theMethodToBeTraced[A <: Int](arg1: => String, arg2: A, arg3: SomeData, arg4: UntraceableData)(using
      arg5: Boolean): String =
    trace5(TraceLevel.Info):
      if arg2 > 2 then "hello"
      else "bye"

object DataFromSignatureUsage:

  @main def dataFromSignature() =
    val usage = new DataFromSignatureUsage

    given Boolean = true

    usage.theMethodToBeTraced("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

// with `UntraceableData` we've opened up a new potential problem
// hardcoding `UntraceableData` into our macro is very inflexible
// what if the user wants to customize what should be traced and what not?
// and how do we decide whether to trace or not types that are not available during
// the compilation of the macro?
