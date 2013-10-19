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
    //create the definitions we're going to add
    val newDefDefs = List(
      DefDef(Modifiers(), newTermName("x"), List(), List(), TypeTree(), Literal(Constant(5))),
      DefDef(Modifiers(), newTermName("y"), List(), List(), TypeTree(), Literal(Constant(7.0f)))
    )

    /*
    this method makes trees explicitly, but as one example, you could do this with reify instead like so:

    val newDefDefs = reify {
      def x = 5
      def y = 7.0f
    }.tree match { case Block(defs, _) => defs}
    */

    //create the parent trait we add
    val addedTrait = Select(Select(Select(
      Select(Ident(newTermName("com")), newTermName("imranrashid")),
      newTermName("oleander")),newTermName("macros")),
      newTypeName("SimpleTrait"))

    //pattern match on the inputs
    val modDefs = inputs map {tree => tree match {
      case ClassDef(mods, name, something, template) =>
        val q = template match {
          case Template(superMaybe, emptyValDef, defs) =>
            //add the trait (if we're supposed to) and the new defs
            val newSuper = if (addSuper) superMaybe ++ List(addedTrait) else superMaybe
            Template(newSuper, emptyValDef, defs ++ newDefDefs)
          case y =>
            y
        }
        ClassDef(mods, name, something, q)
      case x =>
        x
    }}
    //wrap the result up in an Expr, and return it
    val result = c.Expr(Block(modDefs, Literal(Constant())))
    result
  }

  def quasiQuotesImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList

    //you need to put the type in explicitly here with quasiquotes
    val newDefs: List[Tree] = List(
      q"def x = 5",
      q"def y = 7.0f"
    )

    val modDefs = inputs map {tree => tree match {
      case q"class $name extends $parent with ..$traits { ..$body }"=>
        //again, explicit types everywhere with quasiquotes
        val tbody = body.asInstanceOf[List[Tree]]
        val ttraits = traits.asInstanceOf[List[Tree]]
        val q"class $ignore extends $addedType" = q"class Foo extends com.imranrashid.oleander.macros.SimpleTrait"
        val addedTypeList : List[Tree] = List(addedType)
        // and after merging lists together, we need to call .toList again
        q"class $name extends $parent with ..${(ttraits ++ addedTypeList).toList} { ..${(newDefs ++ tbody).toList} }"
      case x =>
        x
    }}
    c.Expr(Block(modDefs, Literal(Constant())))
  }


}
