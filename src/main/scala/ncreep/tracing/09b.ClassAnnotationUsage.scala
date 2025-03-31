package ncreep.tracing


// we exercise the macro annotation with different combinations of methods
// and classes

// first a regular class with the `Info` default
@traceClass(TraceLevel.Info)
class ClassAnnotationUsage:
  // here we provide our own `given` `Tracer` and it will be used
  // as the fallback when tracing methods that don't have their own tracer in scope
  given Tracer = Tracer.make[ClassAnnotationUsage]

  // the fallback tracer should be used here, at the non-default `Debug` level
  @traceAt(TraceLevel.Debug)
  def theMethodToBeTraced1(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
    if arg2 > 2 then "hello"
    else "bye"

  // the argument `Tracer` should be used here, with the fallback tracing level
  def theMethodToBeTraced2(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData)(using
      Tracer): String =
    if arg2 > 2 then "hello"
    else "bye"

@traceClass(TraceLevel.Info)
// we apply the macro to a trait, so we should skip any abstract methods
// and not instrument them
trait TraitAnnotationUsage:
  // there's no given tracer here, so one should be generated as a member and used as fallback

  // should trace with the generated fallback tracer at the default level
  def theMethodToBeTraced1(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
    if arg2 > 2 then "hello"
    else "bye"

  // should not be instrumented in any way
  def methodNotTraced(arg1: String, arg2: Int): Boolean

object ClassAnnotationUsage:

  @main def classAnnotation() =
    val usage1 = new ClassAnnotationUsage

    // should use the default tracer, at the `Debug` tracing level
    usage1.theMethodToBeTraced1("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

    locally:
      // using another marker in the tracing message to distinguish that we
      // are using the correctly scoped tracer
      trait OtherTracer
      given Tracer = Tracer.make[OtherTracer]

      // should use `OtherTracer` at the (default) `Info` level
      usage1.theMethodToBeTraced2("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))

    val usage2 = new TraitAnnotationUsage:
      def methodNotTraced(arg1: String, arg2: Int): Boolean = false

    // should use the generated tracer for the trait at the `Info` level
    usage2.theMethodToBeTraced1("abc", 123, SomeData(4, "password"), UntraceableData('d', "efg"))
