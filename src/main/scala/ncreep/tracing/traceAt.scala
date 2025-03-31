package ncreep.tracing

import scala.annotation.StaticAnnotation
import scala.quoted.*

// an annotation indicating at which tracing level a method should be
// traced when using automatic tracing support
case class traceAt(level: TraceLevel) extends StaticAnnotation

object traceAt:
  given FromExpr[traceAt] with
    def unapply(x: Expr[traceAt])(using Quotes): Option[traceAt] =
      // we match on all the ways to construct a `traceAt` value
      // we can think of and then convert it to the appropriate value
      // note that that `TraceLevel` also has a `FromExpr` definition
      // that's why we can `valueOrAbort` on it
      x match
        case '{ new `traceAt`($level) } => Some(traceAt(level.valueOrAbort))
        case '{ `traceAt`($level) } => Some(traceAt(level.valueOrAbort))
        case _ => None
