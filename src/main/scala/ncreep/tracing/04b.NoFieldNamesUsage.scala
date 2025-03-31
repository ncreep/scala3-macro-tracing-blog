package ncreep.tracing

class NoFieldNamesUsage:
  import NoFieldNames.*

  private given tracer: Tracer = Tracer.make[NoFieldNamesUsage]

  def theMethodToBeTraced(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
    // we can use tracing just as we did before, but now we no longer have to manually
    // repeat the argument names, they are extracted automatically from the name of values
    // (and we still automatically extract the enclosing method name as an ID)
    trace4(TraceLevel.Info, arg1, arg2, arg3, arg4):
      if arg2 > 2 then "hello"
      else "bye"

  // won't compile because the varargs we passed in are not made up of simple identifiers
  // def theMethodToBeTraced2(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
  //   trace4(TraceLevel.Info, arg1, arg2 * 2, arg3, arg4):
  //     if arg2 > 2 then "hello"
  //     else "bye"

object NoFieldNamesUsage:

  @main def noFieldName() =
    val usage = new NoFieldNamesUsage

    usage.theMethodToBeTraced("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

// we can still do better
// the usage pattern we have here is to take all the method's arguments and just pass
// them all along
// instead of repeating the method arguments manually, we can pick them up directly
// from the macro, just like we did with the method name
