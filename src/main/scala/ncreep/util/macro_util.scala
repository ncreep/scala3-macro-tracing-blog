package ncreep.util

import scala.quoted.Quotes
import scala.quoted.*

/** Useful when using the reflection API to figure out hte right types to look at. */
inline def inspectTree[A](inline x: A): A = ${ inspectTreeImpl('x) }

def inspectTreeImpl[A](x: Expr[A])(using Quotes): Expr[A] =
  import quotes.reflect.*

  println("%%%%%%%%%%%%%%%%%")
  println(x.asTerm.show(using Printer.TreeStructure))
  println(x.asTerm.show(using Printer.TreeShortCode))
  println("%%%%%%%%%%%%%%%%%")

  x
