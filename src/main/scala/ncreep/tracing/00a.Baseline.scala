package ncreep.tracing

import scala.util.control.NonFatal

// we are going to explore how we can use macros to simplify the use of
// a semi-realistic tracing library
// we'll see how we can reduce boilerplate using different levels of macro
// implementations, starting from simple but boilerplaty, and ending with
// more complex but almost magical

// but first, a baseline without macros so that we can see what kind of code we
// want to support
object Baseline:
  // we want to trace a block of code with the given metadata
  // that is, we want to surround a call to the block of code with
  // informative calls to the tracing library
  def trace0[A](
      // we have a tracing level, we'll only trace when that level is enabled
      level: TraceLevel,
      // an ID that identifies the code being traced
      // we pass things with by-name params so that we don't actually evaluate
      // them unless tracing is enabled
      id: => String,
      // metadata fields to be added the tracing call, meant to serve
      // as a list of key/value entries
      fields: => (String, String)*)(
      // the actual code being traced, again by-name, as we want to suspend
      // its evaluation to inject tracing before/after it runs
      body: => A)(
      // a tracer, to be filled in by the compiler so as to not bother the users
      // of the tracing library
      using tracer: Tracer): A =
    // we check whether tracing is enabled for the current level
    if tracer.isLevelEnabled(level) then
      // only then we invoke our tracing call with all the metadata
      tracer.start(level, id, fields*)

      // we wrap everything with a `try` so as to be able to catch failures
      // and signal them to the tracer
      try
        // we run the body
        val result = body
        // if we finished successfully, we notify the tracer
        // we add the result as metadata (this is for illustration purposes only
        // using `toString` like this is probably not what you want in a generic context)
        tracer.finish(level, id, "result" -> result.toString)

        // and we return the result to the caller
        result
      catch
        // we catch exceptions
        case NonFatal(e) =>
          // and notify the tracer (again, `toString` for illustration only)
          tracer.finish(level, id, "exception" -> e.toString)

          // and re-throw the exception back to the caller
          throw e
    // if tracing is disabled we run the body without any instrumentation
    else body

// and here's a usage example
class ClassToBeTraced0:
  import Baseline.*

  // we initialize a given tracer
  private given tracer: Tracer = Tracer.make[ClassToBeTraced0]

  // this is the method that we actually want to be traced
  def theMethodToBeTraced(
      arg1: String,
      arg2: Int,
      arg3: SomeData,
      arg4: UntraceableData): String =
    // to trace the invocation of this method we'll use
    // the method name as the ID
    // we'll add all the arguments we got as metadata
    trace0(
      level = TraceLevel.Info,
      id = "theMethodToBeTraced",
      "arg1" -> arg1.toString,
      "arg2" -> arg2.toString,
      "arg3" -> arg3.toString,
      "arg4" -> arg4.toString):
      if arg2 > 2 then "hello"
      else "bye"

  // this code works, but one concern about it is that it might lose on performance
  // as we are wrapping arguments in by-name parameters, using `trace0` might be
  // slower than writing the `if/else` manually every time
  // ideally we would want to avoid the boilerplate but also avoid the performance
  // loss (of course you might not care about performance in this context, and then
  // this code is perfectly fine, but imagine that this is generic library code and you
  // want you user to be able to use it "for free")

  // let's see how we can improve on this
