package ncreep.tracing

import scala.quoted.*
import ncreep.tracing.WithInlining.trace1

// the first step in reducing bolierplate when tracing methods: we are going
// to automatically compute the ID from the name of the method that invoked the
// `trace` method
// (for this series of examples we assume that tracing is going to be invoked at the
// top-level of a method, and will, in effect, be the tracing of the whole method,
// if this is not actually the case, the result from running this macro might
// turn out to be gibberish [like having the generated ID be the name of
// some synthetic method created by the compiler])
object NoMethodName:

  // this is the same as `trace2` but now we no longer have to
  // specify the name of the enclosing method being invoked, so we omit the `ID` argument
  inline def trace3[A](level: TraceLevel, inline fields: (String, String)*)(inline body: A)(using
      tracer: Tracer): A =
    // now we invoke the macro implementation, note how we no longer have an ID argument
    ${ trace3Impl('level, 'fields, 'body, 'tracer) }

  def trace3Impl[A: Type](
      level: Expr[TraceLevel],
      fields: Expr[Seq[(String, String)]],
      body: Expr[A],
      tracer: Expr[Tracer])(using Quotes): Expr[A] =
    // we are going to use some reflection to obtain the name of the enclosing
    // method, hence the magic import
    import quotes.reflect.*

    // we can use `spliceOwner` to obtain the symbol that invoked
    // this macro
    // skipping the direct owner because it's a synthetic symbol
    // a more robust macro might recursively go up until reaching an enclosing method
    // instead we just skip one level up and check that the result is indeed a method
    val methodName = Symbol.spliceOwner.owner match
      // we want to make sure that the enco
      case methodSymbol if methodSymbol.isDefDef =>
        // we obtain the (short) name of the method being traced
        methodSymbol.name
      // in case this is not the case (e.g., the compiler changes the implementation details of
      // macro invocations, or the macro call is not inside a method)
      // we abort with a custom error
      case _ => report.errorAndAbort("`trace` can only be used inside a method")

    // we can now convert into an `Expr` to be spliced in the resulting code
    val id = Expr(methodName)

    // to avoid duplication, we are just going to invoke `trace1` again, except that
    // we pre-populate the `id` argument with the name of the method
    '{ trace1($level, $id, $fields*)($body)(using $tracer) }

    // go to the usage file to see how this works
