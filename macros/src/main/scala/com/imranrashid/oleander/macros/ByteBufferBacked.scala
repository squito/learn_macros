package com.imranrashid.oleander.macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation

class ByteBufferBacked[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ByteBufferBackedImpl.immutable
}

class MutableByteBufferBacked[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ByteBufferBackedImpl.mutable
}

object ByteBufferBackedImpl {
  def immutable(c:Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    expand(c, false)(annottees: _*)
  }

  def mutable(c:Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    expand(c,true)(annottees: _*)
  }

  private def expand(c:Context, includeSetters:Boolean)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val (targetTrait, tpe) = extractAnnotationTypeParameter(c)
    val helper = new BasicReflection(c.universe)
    val targetMethods = helper.targetMethods(tpe.asInstanceOf[helper.u.Type]).map{_.asInstanceOf[MethodSymbol]}.
      toSeq.sortBy{_.name.toString()} //I don't actually care about ordering, but easier for testing

    //each field takes 4 bytes (b/c we're only supporting ints & floats)
    //so go through the fields, and assign each a position offset by 4 bytes.
    val (numBytes,methodToPosition) = getFieldToOffset(c)(targetMethods)

    val newMethods = if (includeSetters) {
      getters(c)(methodToPosition) ++ setters(c)(methodToPosition)
    } else {
      getters(c)(methodToPosition)
    }

    //TODO DRY this part up, I use it a lot -- lots of boilerplate to add more methods & a trait
    val modDefs = annottees.map(_.tree).toList map {tree => tree match {
      case q"class $name($constructor) extends $parent with ..$traits { ..$body }"=>
        //TODO I really need to check the constructor is what I expect ...
        //again, explicit types everywhere with quasiquotes
        val tbody = body.asInstanceOf[List[Tree]]
        val ttraits = traits.asInstanceOf[List[Tree]]
        val addedTypeList : List[Tree] = List(targetTrait)
        // and after merging lists together, we need to call .toList again
        q"class $name($constructor) extends $parent with ..${(ttraits ++ addedTypeList).toList} { ..${(newMethods ++ tbody).toList} }"
      case x =>
        c.abort(tree.pos, "unexpected annottee: " + tree)
    }}
    c.Expr(Block(modDefs, Literal(Constant())))
  }

  def getFieldToOffset(c: Context)(targetMethods: Seq[c.universe.MethodSymbol]): (Int, Map[c.universe.MethodSymbol, Int]) = {
    //each field takes 4 bytes (b/c we're only supporting ints & floats)
    //so go through the fields, and assign each a position offset by 4 bytes.
    targetMethods.foldLeft((0, Map[c.universe.MethodSymbol,Int]())){case((offset, acc), meth) =>
      (offset + 4, acc + (meth -> offset))
    }
  }

  def getters(c:Context)(methodToPosition: Map[c.universe.MethodSymbol, Int]): List[c.universe.Tree] = {
    import c.universe._
    methodToPosition.map{case(m,p) =>
      val name = stringToTermName(m.name.toString)
      m match {
        case intMethod if intMethod.returnType =:= typeOf[Int] =>
          q"def $name = bb.getInt($p)"
        case floatMethod if floatMethod.returnType =:= typeOf[Float] =>
          q"def $name = bb.getFloat($p)"
      }
    }.toList
  }

  def setters(c:Context)(methodToPosition: Map[c.universe.MethodSymbol, Int]): List[c.universe.Tree] = {
    import c.universe._
    methodToPosition.map{case(m,p) =>
      val name = stringToTermName(m.name.toString + "_$eq")
      m match {
        case intMethod if intMethod.returnType =:= typeOf[Int] =>
          q"def $name(v:Int) = bb.putInt($p, v)"
        case floatMethod if floatMethod.returnType =:= typeOf[Float] =>
          q"def $name(v:Float) = bb.putFloat($p, v)"
      }
    }.toList
  }


  def extractAnnotationTypeParameter(c:Context): (c.Tree,c.Type) = {
    import c.universe._
    val targetTrait = c.prefix.tree match {
      case Apply(Select(New(AppliedTypeTree(Ident(_), List(typ))), nme.CONSTRUCTOR), List()) => typ
    }
    val tpe = c.typeCheck(q"(7.asInstanceOf[$targetTrait])").tpe
    (targetTrait, tpe)
  }

}