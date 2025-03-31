package ncreep.tracing

import scala.util.control.NonFatal

// Scala 3 has a builtin way to avoid the performance cost incurred by using by-name
// arguments: `inline`s
object WithInlining:
  // marking all the arguments as `inline` makes our tracing function zero-cost, we
  // no longer have to wrap our arguments in any function wrappers
  // we mark the whole method as inline so as to completely remove any overhead to calling
  // `trace1`
  inline def trace1[A](
      level: TraceLevel,
      // note how instead of `=>` we now use `inline`
      // to test that inlines work properly use a code block as an argument: `{ println("bla"); "abc" }`
      // and see in the generated code how the contents of the block are duplicated into the function call
      inline id: String,
      inline fields: (String, String)*)
  // the `body` is now also `inline`, it will be invoked as is, without the by-name indirection
  (inline body: A)(using tracer: Tracer): A =
    // the implementation is the same as before, as the syntax for using by-name arguments and
    // inlines is the same
    if tracer.isLevelEnabled(level) then
      tracer.start(level, id, fields*)

      try
        val result = body
        tracer.finish(level, id, "result" -> result.toString)

        result
      catch
        case NonFatal(e) =>
          tracer.finish(level, id, "exception" -> e.toString)

          throw e
    else body

end WithInlining

// now we can try actually calling `trace1` and inspect the generated
// code
class ClassToBeTraced1:

  import WithInlining.*

  // we initialize a given tracer
  private given tracer: Tracer = Tracer.make[ClassToBeTraced1]

  // we now want to trace the invocation of this method, so we'll use
  // the method name as the ID
  // we'll add all the arguments we got as metadata
  def theMethodToBeTraced(
      arg1: String,
      arg2: Int,
      arg3: SomeData,
      arg4: UntraceableData): String =
    // if we inspect the generated code, we can see that the `trace1`
    // call disappears completely
    // and the code looks pretty much the same as if someone wrote a manual `if/else`
    // block
    trace1(
      level = TraceLevel.Info,
      id = "theMethodToBeTraced",
      "arg1" -> arg1.toString,
      "arg2" -> arg2.toString,
      "arg3" -> arg3.toString,
      "arg4" -> arg4.toString):
      if arg2 > 2 then "hello"
      else "bye"

    // this pretty much solves any performance issues we had with by-name parameters
    // on other hand, this usage pattern of tracing a method call with its name and
    // arguments seems very repetitive (and error-prone, I had a naming error
    // while refactoring the code)
    // it would be annoying if users had to write out all on their own every time
    // we can try to remove this boilerplate
    // unfortunately, `inline` can't really help us here, as it doesn't have any access
    // to argument names
    // for this we'll have to resort to macros
