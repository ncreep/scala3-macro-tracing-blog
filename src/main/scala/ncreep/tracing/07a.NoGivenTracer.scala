package ncreep.tracing

import scala.quoted.*
import ncreep.tracing.WithInlining.trace1

object NoGivenTracer:
  // before we move on to macro annotations, a quick digression
  // this is essentially the same thing as `trace6` but instead of taking a `given` `Tracer`
  // instance directly it takes an explicit fallback instance
  // the logic will be to try to find an in-scope given instance using `Expr.summon` and only
  // use the fallback instance if a given is not found
  // we won't be using this example directly, but only as helper in examples down the road (hence
  // no usage file)
  // the point of this is that now we can invoke `trace7` in a macro without there being a statically
  // known (at macro compile time) instance of a `Tracer`, and the resolution of the `Tracer`
  // instance will be deferred to macro inlining time in the scope of the caller
  // this will help us circumvent some limitations down the line
  inline def trace7[A](level: TraceLevel, fallbackTracer: Tracer)(inline body: A): A =
    ${ trace7Impl('level, 'fallbackTracer, 'body) }

  def trace7Impl[A: Type](
      level: Expr[TraceLevel],
      fallbackTracer: Expr[Tracer],
      body: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*

    val params = getEnclosingMethodParams
      .filterNot(isGiven)
      .filterNot(isByName)

    val fieldsList = params.flatMap(toField)
    val fields = Expr.ofSeq(fieldsList)

    val name = getEnclosingMethodName
    // here we explicitly summon the `Tracer` but use a fallback in case it's not found
    // it's important that summoning is happening within the scope of the (non-macro) caller
    // of `trace7`
    val tracer = Expr.summon[Tracer].getOrElse(fallbackTracer)

    '{ trace1($level, $name, $fields*)($body)(using $tracer) }
