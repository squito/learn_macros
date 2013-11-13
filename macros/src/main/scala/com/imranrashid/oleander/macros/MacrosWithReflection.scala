package com.imranrashid.oleander.macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation

class FillDefsWithReflection[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro FillDefsWithReflectionImpl.impl
}


object FillDefsWithReflectionImpl {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val targetTrait = c.prefix.tree match {
      case Apply(Select(New(AppliedTypeTree(Ident(_), List(typ))), nme.CONSTRUCTOR), List()) => typ
    }
    val tpe = c.typeCheck(q"(7.asInstanceOf[$targetTrait])").tpe
    val helper = new BasicReflection(c.universe)
    val targetMethods = helper.targetMethods(tpe.asInstanceOf[helper.u.Type])
    val newDefs: List[Tree] = targetMethods.map{m => stringToTermName(m.name.toString) -> m}.map{
      case (name, intMethod) if intMethod.returnType =:= helper.u.typeOf[Int] =>
        q"def $name = 5"
      case (name, floatMethod) if floatMethod.returnType =:= helper.u.typeOf[Float] =>
        q"def $name = 7.0f"
    }.toList

    val modDefs = annottees.map(_.tree).toList map {tree => tree match {
      case q"class $name extends $parent with ..$traits { ..$body }"=>
        //again, explicit types everywhere with quasiquotes
        val tbody = body.asInstanceOf[List[Tree]]
        val ttraits = traits.asInstanceOf[List[Tree]]
        val addedTypeList : List[Tree] = List(targetTrait)
        // and after merging lists together, we need to call .toList again
        q"class $name extends $parent with ..${(ttraits ++ addedTypeList).toList} { ..${(newDefs ++ tbody).toList} }"
      case x =>
        x
    }}
    c.Expr(Block(modDefs, Literal(Constant())))
  }
}