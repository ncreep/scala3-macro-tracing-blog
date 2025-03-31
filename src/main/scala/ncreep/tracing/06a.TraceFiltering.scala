package ncreep.tracing

import scala.quoted.*
import ncreep.tracing.WithInlining.trace1

// we want a more robust way to filter out data that shouldn't be traced
// so now we use the `ToTrace` typeclass, which is just a function to convert
// a value into a string that can be used for tracing output
// we'll use the typeclass to determine which parameters should or should not be traced
// if an instance of `ToTrace` is available for a type then we can trace it
// this way, the user can easily customize and determine which parameters should or
// should not be traced automatically
// also, using `toString` for tracing seems barbaric and crude, `ToTrace` allows us to have
// better control of the tracing output
object TraceFiltering:
  inline def trace6[A](level: TraceLevel)(inline body: A)(using tracer: Tracer): A =
    ${ trace6Impl('level, 'body, 'tracer) }

  def trace6Impl[A: Type](level: Expr[TraceLevel], body: Expr[A], tracer: Expr[Tracer])(using
      Quotes): Expr[A] =
    import quotes.reflect.*

    // `getEnclosingMethodParams` is the same logic that we used in the previous example
    // so we get a list of symbols for all the parameters of the method
    val params = getEnclosingMethodParams
      // yet again, we filter away given and by-name parameters
      // but we do not explicitly filter out `UntraceableData`
      .filterNot(isGiven)
      .filterNot(isByName)

    // we again convert every parameter into a field but this
    // time we are going to use the `ToTrace` typeclass to determine
    // how to trace each parameter
    // jump to the definition of `toField`
    // note that we are using `flatMap` here, as `toField` can
    // potentially filter out some of the parameters (by returning `None`)
    // but we no longer have to explicitly hardcode `UntraceableData` into the
    // macro's logic
    val fieldsList = params.flatMap(toField)

    // this is the same logic as we had before, turn everything
    // into a single list, and get the name of the enclosing method
    val fields = Expr.ofSeq(fieldsList)

    val name = getEnclosingMethodName

    // everything is ready, we can call `trace1`
    '{ trace1($level, $name, $fields*)($body)(using $tracer) }
