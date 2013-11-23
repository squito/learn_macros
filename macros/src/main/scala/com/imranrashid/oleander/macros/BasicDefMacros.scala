package com.imranrashid.oleander.macros

import language.experimental.macros
import scala.reflect.macros.Context

/**
 *
 */
object BasicDefMacros {
  def hello(stuff: Any) = macro helloImpl

  def helloImpl(c:Context)(stuff:c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    reify{"hello " + stuff.splice}
  }

  def errorIfNotConstant(stuff:Any) = macro errorImpl

  def errorImpl(c:Context)(stuff: c.Expr[Any]) : c.Expr[Any] = {
    import c.universe._
    stuff.tree match {
      case Literal(Constant(c)) => reify{5}
      case _ => c.abort(c.enclosingPosition, "you must pass a constant to this macro")
    }
  }

}
