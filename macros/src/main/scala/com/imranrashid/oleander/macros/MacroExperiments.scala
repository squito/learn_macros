package com.imranrashid.oleander.macros


import language.experimental.macros
import reflect.macros.Context
import scala.annotation.StaticAnnotation



/*
 hodge podge of stuff I played with to learn macros.  mostly a workbook for pasting into the repl
 */

object MacroTests {
  def printMacro(s: String) = macro printMacroImpl

  def printMacroImpl(c: Context)(s: c.Expr[String]) : c.Expr[Any] = {
    import c.universe._
    println(showRaw(reify{"x " + 5}))
    val x = c.Expr(Apply(Select(Literal(Constant("x ")), newTermName("$plus")), List(s.tree)))
    reify{println(x.splice)}
  }

  def printTreeImpl(c: Context)(stuff: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    println(showRaw(stuff.tree))
    reify{()}
  }

  def printTree(stuff: Any) = macro printTreeImpl


  def valDefImpl(c: Context)(ignore: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val valName = c.fresh("blah")
    println("valName = " + valName)
    reify{c.Expr[Unit](Block(List(ValDef(Modifiers(), valName, TypeTree(), Literal(Constant(17)))), Literal(Constant()))).splice}
  }

  def valDef(ignore: Any): Unit = macro valDefImpl


  def valDefQuasiImpl(c: Context)(ignore: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    reify{c.Expr[Any](q"val x = 17").splice}
  }

  def valDefQuasi(ignore: Any) = macro valDefQuasiImpl



  def classExpandMacroImpl(c: Context)(s: c.Expr[Any]) : c.Expr[Any] = {
    //this wouldn't actually work, b/c I only end up defining a class *within* this block
    // though, this "slays the compiler" anyway

    import c.universe._

    val cdef = s.tree match {
      case Block(List(x:ClassDef), _) => x
      case _ => c.abort(c.enclosingPosition, "Was expecting a block w/ a ClassDef")
    }

    val q"class $name { ..$body }" = cdef
    val newdefs = List[c.universe.Tree](q"def x: Int = z + 7", q"def y: Float = z + 3.2f")
    val mergedefs = body.asInstanceOf[List[c.universe.Tree]] ++ newdefs
    println(showRaw(q"class $name extends AnyRef { ..${mergedefs}}"))
//    reify{()}
    reify{c.Expr[ClassDef](q"class $name extends AnyRef { ..${mergedefs}}").splice}
  }

  def classExpandMacro(s:Any) = macro classExpandMacroImpl

}


trait Blah {
  def x: Int
  def y: Int
}

class BlahFiller extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro FillerImpl.impl
}

object FillerImpl {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val newDefDef = DefDef(Modifiers(), newTermName("x"), List(), List(), TypeTree(), Literal(Constant(5)))
    val modDefs = inputs map {tree => tree match {
      case ClassDef(mods, name, something, template) =>
        val q = template match {
          case Template(superMaybe, emptyValDef, defs) =>
            Template(superMaybe, emptyValDef, newDefDef::defs)
          case y =>
            y
        }
        ClassDef(mods, name, something, q)
      case x =>
        x
    }}
    val result = c.Expr(Block(modDefs, Literal(Constant())))
    result
  }
}
