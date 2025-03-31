package ncreep.tracing

import scala.quoted.*
import ncreep.tracing.NoMethodName.trace3Impl

// the same as `trace3` but now we longer have to name the arguments on our own
// we just pick up the names of the `val`s that are passed to the tracing method
// e.g.:
// ```
// trace4(TraceLevel.Info, arg1, arg2, arg3)
// ```
// will pick up the `argN` value names and pass them along for tracing
object NoFieldNames:
  // instead of passing key/value pairs for the fields, we now pass a single
  // vararg with only the field values
  // we mark it `inline` so that we can inspect the contents of the varargs and
  // extract the field names from them
  inline def trace4[A](level: TraceLevel, inline fieldValues: Any*)(inline body: A)(using
      tracer: Tracer): A =
    ${ trace4Impl('level, 'fieldValues, 'body, 'tracer) }

  def trace4Impl[A: Type](
      level: Expr[TraceLevel],
      fieldValues: Expr[Seq[Any]],
      body: Expr[A],
      tracer: Expr[Tracer])(using Quotes): Expr[A] =
    import quotes.reflect.*

    // first step, we want to extract each expression that
    // was passed as a vararg into a separate `Expr` instance
    // we use the custom `Varargs` extractor for this
    // notice the change in types, we flipped the two layers
    val varargExpressions: Seq[Expr[Any]] = fieldValues match
      case Varargs(argExprs) => argExprs // this is now a list of `Expr`s
      case _ => // probably can't ever happen, but we can't prove it
        report.errorAndAbort("`fieldValues` must be varargs")

    // now we want to find the name of each argument we got and pair it with
    // that argument
    val fieldsWithNames = varargExpressions.map: arg =>
      // we convert to a `Term` so that we can match on it and extract its name
      arg.asTerm match
        // here we only support simple identifiers as names, given an
        // identifier we match on it and take its name as a string
        // (it's conceivable to support more complicated arguments as well
        // but there would open a question of how to name them in the tracing output)
        case Ident(name) =>
          val fieldName = Expr(name)

          // we now create quoted tuple, where we splice the name of the identifier
          // along with the value we are going to pass into tracing
          // note how this recreates the logic we manually had to write for each field
          '{ $fieldName -> $arg.toString }
        // in case we got anything that's not a simple identifier, we abort compilation
        case _ => report.errorAndAbort("field names must be simple identifiers")

    // we now go back to an `Expr[Seq[...]]`, again, note the layer flip
    val fields: Expr[Seq[(String, String)]] = Expr.ofSeq(fieldsWithNames)

    // now we can pass this along to the implementation of `trace3` macro
    // this way we don't have to rewrite the method name extraction logic
    // but we pre-populate the fields with the ones we extracted from the varargs
    trace3Impl(level, fields, body, tracer)
