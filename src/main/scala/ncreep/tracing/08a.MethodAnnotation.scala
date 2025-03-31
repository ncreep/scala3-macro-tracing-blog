package ncreep.tracing

import scala.quoted.*
import scala.annotation.MacroAnnotation
import ncreep.tracing.TraceFiltering.trace6

// we are now going to define a macro annotation that will fully automate the
// tracing process without directly touching the body of the method
// we still need to be able to choose the tracing level, so we are going to pass
// it as an argument directly to the annotation
class traceMethod(level: TraceLevel) extends MacroAnnotation:

  def transform(using Quotes)(
      definition: quotes.reflect.Definition,
      companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect.*

    // in this annotation we are only going to support non-abstract methods, so we match
    // on the provided definition
    definition match
      // we catch the `def`
      // the presence of `rhsTree` indicates that the method is not abstract
      case defDef @ DefDef(name, _, _, Some(rhsTree)) =>
        // to be able to use the `level` in the quotes we are about to write,
        // we need to turn it into an `Expr`, for that purpose we defined a
        // `ToExpr` for `TraceLevel`, jump to the `common.scala` file to see how it works
        val levelExpr = Expr(level)
        // we extract the name of the method directly from the match (rather than using its
        // `Symbol`)
        val nameExpr = Expr(name)

        // to be able do any tracing we need an instance of a `Tracer`
        // for that we are going to try and summon a `Tracer` instance
        // what we want to achieve is that we get the `given` tracer that would've
        // been available had this code been written directly in the method that was annotated
        // for that we use `summonInline`
        // relying on the fact that `summonInline` should behave like `Expr.summon`
        // but this way the summoning is happening in the correct scope (the method's inner scope)
        // and also saves us from coming up with our own error message
        val tracer = '{ compiletime.summonInline[Tracer] }

        // now we are going to rewrite the implementation of the method we are instrumenting
        // since `rhsTree` is a `Term` we cast it into an `Expr`
        // but the cast ends up with having an `Expr[?]`
        // to be able to maintain the precise type we started with we are matching on the
        // `Expr` and assigning a type to
        val newRhs = rhsTree.asExpr match
          // by matching on the expression this way we are getting the exact type
          // of the expression, although we do not currently use it we have to name it
          // otherwise the compiler will infer `Any` and fail to typecheck
          case '{ $body: t } =>
            // and now for the actual new implementation of the function
            // we just call `trace6` our most automated def macro and let
            // it fully instrument the body of the method
            // using the macro call rather than the "impl" because we want
            // the macro to properly initialize a `Quotes` instance in the right scope
            // (or else the `owner` of this macro call and various givens might not resolve correctly)
            '{ trace6($levelExpr)($body)(using $tracer) }

        val newTree = DefDef(defDef.symbol, _ => Some(newRhs.asTerm))

        List(newTree)

      case _ =>
        // in case the annotation was applied to anything else, we gracefully error out
        report.errorAndAbort("Tracing is only supported on a non-abstract `def`")
