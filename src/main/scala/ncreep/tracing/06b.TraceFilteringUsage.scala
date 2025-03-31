package ncreep.tracing


class TraceFilteringUsage:
  import TraceFiltering.*

  given tracer: Tracer = Tracer.make[TraceFilteringUsage]

  // tracing now works the same as before, but `UntraceableData` is omitted because it
  // doesn't have a `ToTrace` instance in scope
  // additionally, if we run this example we can see that one of the fields of `SomeData`
  // (suggestively named `sensitiveDoNotLog`) appears as "redacted", this is a customization
  // that we applied using `SomeData`'s `ToTrace` instance
  def theMethodToBeTraced(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
    trace6(TraceLevel.Info):
      if arg2 > 2 then "hello"
      else "bye"

object TraceFilteringUsage:

  @main def traceFiltering() =
    val usage = new TraceFilteringUsage

    usage.theMethodToBeTraced("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

// we got pretty much as far as we can go with automating boilerplate with a `def` macro
// but still, this is not a smooth as it can be, we still have to modify the body of the
// method just to apply automated tracing to it
