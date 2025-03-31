package ncreep.tracing


class MethodAnnotationUsage:
  // this is the tracer that's going to be used by default within this class
  // if we comment this out the code won't compile
  // unfortunately the error message seems to point directly into the macro's code
  // rather than showing the correct location where the macro is actually expanded
  given Tracer = Tracer.make[ClassAnnotationUsage]

  // we'll be tracing this method at the debug level
  @traceMethod(TraceLevel.Debug)
  def theMethodToBeTraced1(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
    if arg2 > 2 then "hello"
    else "bye"

  // and this method at the info level
  @traceMethod(TraceLevel.Info)
  def theMethodToBeTraced2(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData)(
      // within this method we should be using the more specific tracer passed here, rather
      // than the default one defined above
      using Tracer): String =
    if arg2 > 2 then "hello"
    else "bye"

object MethodAnnotationUsage:

  @main def methodAnnotation() =
    val usage = new MethodAnnotationUsage

    // when running the code note that we are using the correct tracing level
    usage.theMethodToBeTraced1("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

    locally:
      // using another marker in the tracing message to distinguish that we
      // are using the correctly scoped tracer
      trait OtherTracer
      given Tracer = Tracer.make[OtherTracer]

      // note that `OtherTracer` is picked up by the macro
      usage.theMethodToBeTraced2("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

// we are getting very close to auto-tracing nirvana
// but there's still some repetition here
// we can imagine wanting to trace all the methods of a whole class
// why should we be repeatedly marking it one by one?
