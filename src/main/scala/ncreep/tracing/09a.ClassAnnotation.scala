package ncreep.tracing

import scala.annotation.MacroAnnotation
import scala.annotation.StaticAnnotation
import scala.quoted.*

// to make everything fully automatic, we're going to define
// a macro annotation that works on class definitions, automatically
// applying tracing to all methods of a class (we could've done
// that with the method annotation as well, just by adding another branch to
// the pattern match there, we split things up for pedagogical reasons)

// the logic is as follows:
// - the annotation has an argument specifying the default tracing level
// - for every non-abstract method on the class
// - check whether the class has the `traceAt` annotation
// - if it does, apply tracing to the method at the level
//   specified by the `traceAt` annotation
// - if not, apply tracing to the method with the default level
// (as we are venturing into more experimental realms, more things
// tend to break)
class traceClass(defaultLevel: TraceLevel) extends MacroAnnotation:
  import NoGivenTracer.trace7

  def transform(using Quotes)(
      definition: quotes.reflect.Definition,
      companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect.*

    definition match
      // we match only on class definitions (this includes traits as well)
      case cls @ ClassDef(_, _, parents, _, body) =>
        // to be able to support tracing within the class we want to make sure
        // that the class contains a tracer in scope
        // if a `given` tracer was defined on the class, we want to use
        // that
        // otherwise, we will create a new (private) `Tracer` as a private member
        // of the class (recall that macros cannot expose new definitions to the outside
        // world)
        // in the latter case we'll need to keep the definition of the new `Tracer`
        // so that we can later add it to the class
        // the result is going to be a fallback tracer that we can use for tracing
        // in the different methods in the class
        // it's a fallback in the sense that it will only be used within a method
        // if the method doesn't have its own, more specific, `given` tracer
        // move over to `getOrCreateTracer` to see how this works
        val (fallbackTracer, tracerDefinition) = getOrCreateTracer(cls.symbol)

        // with the fallback `Tracer` in hand, along with default tracing level
        // from the class's constructor
        // we can use them to instrument all the methods in the class
        // we do this by manually passing around the fallback `Tracer`
        // in a perfect world we would use a `given` to pass the fallback around
        // unfortunately, I couldn't get the macro to consider the macro-added
        // `given` definition when resolving the `Tracer`
        // so instead we are passing it manually
        // tried something along these lines without success:
        // https://github.com/lampepfl/dotty/issues/12359
        // jump to `traceMethods` to see how we add tracing
        val withTracedMethods = traceMethods(body, fallbackTracer, defaultLevel)

        // now we produce the list of definitions as the output of the macro
        // if we created a new `Tracer` we want to add its definition to the output,
        // along with the new definitions of the instrumented method
        // note that if we forget to add the newly created symbol to the class, it will
        // still compile, but will fail at runtime, as the symbol will be missing then
        val newBody = tracerDefinition.toList ++ withTracedMethods

        val newClass = ClassDef(cls.symbol, parents, newBody)

        // return the redefined class as the output of the macro
        List(newClass)

      case _ =>
        // fail to compile on any other definition
        report.errorAndAbort("Tracing is only supported on classes")

  // a method that creates a new `Tracer` definition using the symbol of class as
  // an argument
  // the tracer is only created if no given `Tracer` exists in scope
  // if a given tracer does exist, it will be returned instead
  def getOrCreateTracer(using
      Quotes)(cls: quotes.reflect.Symbol): (Expr[Tracer], Option[quotes.reflect.ValDef]) =
    import quotes.reflect.*

    // we are trying to obtain a given tracer within the scope of the class
    // we are instrumenting
    // will be `None` if no such tracer exists
    val maybeTracer =
      // we want the given scope to be the scope of the class we are currently processing
      // if we use the `Quotes` instance provided above we will be in the package scope
      given Quotes = cls.asQuotes
      Expr.summon[Tracer]

    // we are branching on whether we found a `tracer` or not
    maybeTracer match
      // a provided `Tracer` was found, we are going to return it and
      // not create any new definitions (the `None` part of the result)
      case Some(tracer) => (tracer, None)

      // no given `Tracer` was found, we are going to create a
      // new member with a `Tracer` in it
      case None =>
        // this is the code defining the new tracer, jump there
        // to see the details
        val tracerDefinition = makeTracer(cls)

        // a reference to the tracer to be used by the instrumentation
        // code, we must not forget to add the definition for this
        // value to the output of the macro
        val tracerRef = Ref(tracerDefinition.symbol).asExprOf[Tracer]

        // returning both a reference to the newly created `Tracer` and
        // its definition
        (tracerRef, Some(tracerDefinition))

  // creating a new field of type `Tracer` within the scope of the
  // given class
  def makeTracer(using Quotes)(cls: quotes.reflect.Symbol) =
    import quotes.reflect.*

    // a brand new `val`, first we need a `Symbol` for it
    val tracerSymbol =
      Symbol.newVal(
        // we nest it within the provided class
        parent = cls,
        // to avoid possible name clashes, we are using a fresh name
        name = Symbol.freshName("tracer"),
        tpe = TypeRepr.of[Tracer],
        flags =
          // although it makes sense to make this definition lazy (it might not be used if all methods do not require
          // a tracer)
          // but it's actually mandatory, otherwise the macro won't work for traits and will crash the compiler
          // not adding a `given` flag, since we are going to pass this value manually
          Flags.Final | Flags.Lazy | Flags.Private,
        privateWithin = Symbol.noSymbol)

    // now we need to actually define how to create the `Tracer`
    val tracerRhs =
      // making sure to use the correct scope when creating quotes
      given Quotes = tracerSymbol.asQuotes

      // since the class is passed in "dynamically" we need to match on
      // its type to have a name for it
      // `typeRef` gives us a `TypeRepr` corresponding to this class
      cls.typeRef.asType match
        case '[t] =>
          // we create a new `Tracer` for the class
          // to be able to create it we need to provide a `ClassTag`
          // since we do not statically know the type here, we use `summonInline`
          // to be evaluated later on when the macro is expanded and the
          // actual type is known
          '{ Tracer.make[t](using compiletime.summonInline) }.asTerm

    // assemble the symbol and the RHS into a single definition
    val tracerVal = ValDef(tracerSymbol, Some(tracerRhs))

    tracerVal

  // add instrumentation to all the methods within the body of the class
  def traceMethods(using Quotes)(
      // the definitions of the methods to be instrumented
      body: List[quotes.reflect.Statement],
      // a fallback tracer to be used in case a method doesn't have
      // its own `Tracer` in the `given` scope
      fallbackTracer: Expr[Tracer],
      // the default level for tracing, in case no explicit level is
      // provided for the method
      defaultLevel: TraceLevel) =
    import quotes.reflect.*

    // for each definition
    body.map:
      // we apply tracing only to non-abstract methods (for abstract methods
      // `rhsTree` would be `None`)
      case defDef @ DefDef(name, _, _, Some(rhsTree)) =>
        // we need to know at which tracing level we should apply tracing
        // first try to obtain the level directly from the `def` itself (via
        // the `traceAt` annotation) or use the default level
        // move to `getTracingLevel` to see how to analyze the def's annotations
        // to be able to use `Expr.apply` on `TraceLevel` we need a
        // `ToExpr` instance, which you can see in the `common.scala` file
        val tracingLevel = Expr(getTracingLevel(defDef.symbol).getOrElse(defaultLevel))

        // yet again we have the body of a method as an untyped `Term`
        // we convert it into an `Expr` and then match it to obtain its precise
        // type to be used by the quotes below
        val newRhs = rhsTree.asExpr match
          // we have a tracing level and a fallback tracer
          // we can use them to instrument the body of the method
          // this is where `trace7` comes in
          // recall that `trace7` has similar logic to our full instrumentation (`trace6`)
          // but instead of using a given `Tracer` it tries to resolve one within the
          // body of `trace7` and use a fallback in case no `Tracer` was found
          // due to limitation with macro-generated `given` members mentioned above (they are
          // not discovered when resolving givens inside a method) we have to do
          // this manual plumbing of the `Tracer` value rather than just relying
          // on the regular `given`s mechanism
          // the result of `trace7` is a fully traced method that uses the fallback
          // tracer only when no given `Tracer` is found in the scope of the method
          case '{ $body: t } => '{ trace7($tracingLevel, $fallbackTracer)($body) }

        // copying the def with the new, instrumented body
        DefDef(defDef.symbol, _ => Some(newRhs.asTerm))
      // for anything that's not a non-abstract method we just return
      // it untouched
      case notMethod => notMethod

  // obtaining the appropriate tracing level for a method
  // we use the `traceAt` annotation to figure it out
  def getTracingLevel(using Quotes)(defSym: quotes.reflect.Symbol): Option[TraceLevel] =
    import quotes.reflect.*

    // to be able to find a specific annotation we need a `Symbol` for
    // it, which we get from its `TypeRepr`
    val traceAtSymbol = TypeRepr.of[traceAt].typeSymbol

    defSym
      // we query for annotations with the symbol we specified
      .getAnnotation(traceAtSymbol)
      // if the annotation is present, we have access to the `Term` that defines it
      // we want to extract the `TraceLevel` that it contains
      .map: annotationTerm =>
        // first we cast into `traceAt`, safe since this is how we obtained
        // this symbol to begin with
        // then we rely on `traceAt`'s `FromExpr` instance to extract
        // the value to be available for the macro at compile-time
        // we could skip this, and just use the annotation as an `Expr` when
        // creating quoted code as output
        // but if we keep the `Expr` with the annotation, we will get a redundant instantiation
        // of the annotation object at runtime
        // instead we extract the actual tracing-level at compile-time and inject it directly
        // into the generated code completely discarding the `traceAt` annotation, so that
        // it doesn't appear at runtime
        // it can be instructive to try to use the `Expr` `'{ traceAt.level }` and see the
        // generated code
        val traceAt = annotationTerm.asExprOf[traceAt].valueOrAbort

        // we now (optionally) have the tracing level for the
        // method available at compile-time for the macro to use
        traceAt.level
