package ncreep.tracing

class NoMethodNameUsage:
  import NoMethodName.*

  private given tracer: Tracer = Tracer.make[NoMethodNameUsage]

  def theMethodToBeTraced(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
    // this is the same the usage example before, but we no longer pass in the method name
    // see in the generated code how it's filled in
    trace3(
      TraceLevel.Info,
      "arg1" -> arg1.toString,
      "arg2" -> arg2.toString,
      "arg3" -> arg3.toString,
      "arg4" -> arg4.toString):
      if arg2 > 2 then "hello"
      else "bye"

  // won't compile because we're not within a method
  // val theValToBeTraced: String =
  //   trace3( TraceLevel.Info):
  //     if 3 > 2 then "hello"
  //     else "bye"

object NoMethodNameUsage:

  @main def noMethodName() =
    val usage = new NoMethodNameUsage

    usage.theMethodToBeTraced("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

  // the call above is an improvement, but we can do better
  // passing in all the method arguments with their names can also
  // be automated by the macro
