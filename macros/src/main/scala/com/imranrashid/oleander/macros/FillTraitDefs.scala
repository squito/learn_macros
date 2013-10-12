package com.imranrashid.oleander.macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation

class FillTraitDefs extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro SimpleTraitImpl.addDefs
}

class AddTraitAsSuper extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro SimpleTraitImpl.addDefsAndTrait
}

class QuasiQuoteAddTrait extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro SimpleTraitImpl.quasiQuotesImpl
}

trait SimpleTrait {
  def x: Int
  def y: Float
}

object SimpleTraitImpl {

  def addDefs(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    impl(c)(false, annottees: _*)
  }

  def addDefsAndTrait(c:Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    impl(c)(true, annottees: _*)
  }

  def impl(c: Context)(addSuper: Boolean, annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val newDefDefs = List(
      DefDef(Modifiers(), newTermName("x"), List(), List(), TypeTree(), Literal(Constant(5))),
      DefDef(Modifiers(), newTermName("y"), List(), List(), TypeTree(), Literal(Constant(7.0f)))
    )
//    val addedTrait = TypeTree().setOriginal(Select(Select(Ident("com"), "com.imranrashid"), "com.imranrashid.ATrait"))
//    val addedTrait = TypeTree().setOriginal(Select(Select(Ident("ooga"), "booga"), "Wakka"))
//    val addedTrait = TypeTree().setOriginal(Ident("TopLevelTrait"))
//    val addedTrait = Ident(newTypeName("TopLevelTrait"))  // WORKS!!! discovered via -Yshow-trees-compact
    val addedTrait = Select(Select(Select(
      Select(Ident(newTermName("com")), newTermName("imranrashid")),
      newTermName("oleander")),newTermName("macros")),
      newTypeName("SimpleTrait"))
    /* doesn't really work.  results in:


<console>:44: warning: method Select in trait Trees is deprecated: Use Select(tree, newTermName(name)) instead
           val addedTrait = TypeTree().setOriginal(Select(Select(Ident("ooga"), "ooga.booga"), "ooga.booga.Wakka"))
                                                   ^
<console>:44: warning: method Select in trait Trees is deprecated: Use Select(tree, newTermName(name)) instead
           val addedTrait = TypeTree().setOriginal(Select(Select(Ident("ooga"), "ooga.booga"), "ooga.booga.Wakka"))
                                                          ^
<console>:44: warning: method Ident in trait Trees is deprecated: Use Ident(newTermName(name)) instead
           val addedTrait = TypeTree().setOriginal(Select(Select(Ident("ooga"), "ooga.booga"), "ooga.booga.Wakka"))

and then

scala> @AddTraitAsSuper class Blarg
error: object Wakka is not a member of package ooga.booga
Note: trait Wakka exists, but it has no companion object.

or

scala> @AddTraitAsSuper class Blarg
<console>:16: error: class type required but TopLevelTrait.type found
       @AddTraitAsSuper class Blarg
        ^
     */

    val modDefs = inputs map {tree => tree match {
      case ClassDef(mods, name, something, template) =>
        val q = template match {
          case Template(superMaybe, emptyValDef, defs) =>
            val newSuper = if (addSuper) superMaybe ++ List(addedTrait) else superMaybe
            Template(newSuper, emptyValDef, defs ++ newDefDefs)
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

  def quasiQuotesImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList

    val newDefs: List[Tree] = List(
      q"def x = 5",
      q"def y = 7.0f"
    )

    val modDefs = inputs map {tree => tree match {
      case q"class $name extends $parent with ..$traits { ..$body }"=>
        val tbody = body.asInstanceOf[List[Tree]]
        val ttraits = traits.asInstanceOf[List[Tree]]
        val q"class $ignore extends $addedType" = q"class Foo extends com.imranrashid.oleander.macros.SimpleTrait"
        val addedTypeList : List[Tree] = List(addedType)
        q"class $name extends $parent with ..${(ttraits ++ addedTypeList).toList} { ..${(newDefs ++ tbody).toList} }"
      case x =>
        x
    }}
    c.Expr(Block(modDefs, Literal(Constant())))
  }
}
