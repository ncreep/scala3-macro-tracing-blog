package ncreep.tracing

// the usage of the macro is the same as `trace1` except we are forced to run in
// separate file (an implementation limitation of macros)
// the generated code is still the same thing
class BasicMacroUsage:
  import BasicMacro.*

  given tracer: Tracer = Tracer.make[BasicMacroUsage]

  def theMethodToBeTraced(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
    trace2(
      TraceLevel.Info,
      "theMethodToBeTraced",
      "arg1" -> arg1.toString,
      "arg2" -> arg2.toString,
      "arg3" -> arg3.toString,
      "arg4" -> arg4.toString):
      if arg2 > 2 then "hello"
      else "bye"

object BasicMacroUsage:

  @main def basicMacro() =
    val usage = new BasicMacroUsage

    usage.theMethodToBeTraced("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

// we just dipped our toes into a macro, but we didn't actually improve
// anything
// let's make the first step in that direction
