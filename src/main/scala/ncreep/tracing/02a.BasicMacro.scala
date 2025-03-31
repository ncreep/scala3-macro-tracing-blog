package ncreep.tracing

import scala.quoted.*
import scala.util.control.NonFatal

// first, we'll reimplement the logic from `trace1` using a macro
// this serves as a reminder to macro syntax, but is not actually necessary,
// as the result is the same thing as `trace1`, we don't add any more magic at
// the moment
object BasicMacro:
  inline def trace2[A](
      level: TraceLevel,
      // important: without inlining these arguments will be evaluated before the macro code is evaluated
      // https://docs.scala-lang.org/scala3/guides/macros/inline.html#semantics-of-inline-methods
      // this differs from how it works in Scala 2
      // so even though we don't intend to inspect the `Expr`s for the arguments, we still need
      // to mark the `inline` even when the method is actually a macro
      inline id: String,
      inline fields: (String, String)*)(inline body: A)(using tracer: Tracer): A =
    // and now we proceed to invoke the macro with all the arguments quoted
    ${ trace2Impl('level, 'id, 'fields, 'body, 'tracer) }

  // note that the `Type` constraint that we must provide here, since we are using
  // a type parameter
  def trace2Impl[A: Type](
      level: Expr[TraceLevel],
      id: Expr[String],
      fields: Expr[Seq[(String, String)]],
      body: Expr[A],
      tracer: Expr[Tracer])(using Quotes): Expr[A] =
    // this is the same code as the `trace1` but now it's all in quotes and
    // splices
    // the final generated code will be the same
    '{
      if $tracer.isLevelEnabled($level) then
        $tracer.start($level, $id, $fields*)

        try
          val result = $body
          $tracer.finish($level, $id, "result" -> result.toString)

          result
        catch
          case NonFatal(e) =>
            $tracer.finish($level, $id, "exception" -> e.toString)

            throw e
      else $body
    }

    // head over to the usage file to see how this works
