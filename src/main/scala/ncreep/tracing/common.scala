package ncreep.tracing

import scala.quoted.*

// Various helpers

// The return type here relies on givens being in scope, as per
// https://docs.scala-lang.org/scala3/guides/macros/reflection.html#macro-api-design
def getEnclosingMethodSymbol(using Quotes): quotes.reflect.Symbol =
  import quotes.reflect.*

  // skipping the direct owner because it's a synthetic symbol
  // a more robust macro might recursively go up until reaching an enclosing method
  // instead we just skip one level up and check that the result is indeed a method
  Symbol.spliceOwner.owner match
    case methodSymbol if methodSymbol.isDefDef => methodSymbol
    case _ => report.errorAndAbort("trace can only be used at the top-level of a method")

// getting the list of parameter of the method enclosing this macro
// invocation
def getEnclosingMethodParams(using Quotes) =
  getEnclosingMethodSymbol.paramSymss.flatten.filter(_.isTerm)

// getting the name of the method enclosing this macro invocation
def getEnclosingMethodName(using Quotes): Expr[String] =
  Expr(getEnclosingMethodSymbol.name)

// checking whether a `Symbol` was marked as `given`
def isGiven(using Quotes)(param: quotes.reflect.Symbol) =
  import quotes.reflect.*

  // extract the symbol's flags
  val flags = param.flags

  // verify that it's neither `given` nor `implicit` (for legacy reasons)
  flags.is(Flags.Given) || flags.is(Flags.Implicit)

// checking whether a symbol has a by-name type
def isByName(using Quotes)(param: quotes.reflect.Symbol) =
  import quotes.reflect.*

  // we match on the symbol's `TypeRepr` and see whether it has the `ByNameType`
  // shape
  param.info match
    case ByNameType(_) => true
    case _ => false

// converting a symbol of a parameter into a tracing field (a pair of name
// and string description)
def toField(using Quotes)(param: quotes.reflect.Symbol): Option[Expr[(String, String)]] =
  import quotes.reflect.*

  // we are again converting method parameters into tracing fields in the form
  // of `name -> value`
  val fieldName = Expr(param.name)

  // we need the type of the parameter so that we can try
  // to summon a `ToTrace` instance for it
  val paramType = param.info.asType

  val maybeField = paramType match
    // we match on the unknown type so that we can name it without manual tree construction
    case '[t] =>
      // we summon an instance of `ToTrace` for this unknown type
      // the result is an `Option`, which will be `None` if no instance
      // was found for `t`
      val maybeTraceable = Expr.summon[ToTrace[t]]
      // we create a reference to the parameter
      // since we need the types to align in the quote below we are casting
      // it to `t`, which is safe, since we got this type from matching on this very
      // symbol's type
      val fieldValue = Ref(param).asExprOf[t]

      // if we do indeed have a `ToTrace` instance, we are going to
      // call it with the parameter's value
      // but if we got a `None` here we are just going to skip this parameter
      // entirely
      maybeTraceable.map: traceable =>
        '{ $fieldName -> $traceable.toTraceString($fieldValue) }

  maybeField
end toField

given ToExpr[TraceLevel] with

  def apply(level: TraceLevel)(using Quotes): Expr[TraceLevel] =
    // we match on the level and create a corresponding quote for each one
    level match
      case TraceLevel.Debug => '{ TraceLevel.Debug }
      case TraceLevel.Info => '{ TraceLevel.Info }
      case TraceLevel.Warn => '{ TraceLevel.Warn }
      case TraceLevel.Error => '{ TraceLevel.Error }

given FromExpr[TraceLevel] with
  def unapply(x: Expr[TraceLevel])(using Quotes): Option[TraceLevel] =
    // note that nothing is forcing us to check all the cases, if we add a new
    // case, this will fail silently, there's no way to turn on exhaustivity checking
    x match
      case '{ TraceLevel.Debug } => Some(TraceLevel.Debug)
      case '{ TraceLevel.Info } => Some(TraceLevel.Info)
      case '{ TraceLevel.Warn } => Some(TraceLevel.Warn)
      case '{ TraceLevel.Error } => Some(TraceLevel.Error)
      case _ => None
