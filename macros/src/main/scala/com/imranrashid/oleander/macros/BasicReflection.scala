package com.imranrashid.oleander.macros

class BasicReflection(val u: scala.reflect.api.Universe) {

  import u._

  def zeroArgMethods(typ: Type) = {
    typ.members.collect{
      case m if m.isMethod => m.asMethod
    }.filter{_.paramss.isEmpty}
  }

  def undefinedZeroArgMethods(typ: u.Type) = {
    zeroArgMethods(typ).filter{!isDefined(_)}
  }

  def targetMethods(typ: u.Type) = {
    undefinedZeroArgMethods(typ).filter{m =>
      val rt = m.returnType
      (rt =:= u.typeOf[Int]) || (rt =:= u.typeOf[Float])
    }
  }

  def isDefined(method: u.MethodSymbol): Boolean = {
    //from http://stackoverflow.com/questions/16792824/test-whether-a-method-is-defined
    !isDeferred(method)
  }

  def isDeferred(sym: u.MethodSymbol) = sym
    .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    .hasFlag(scala.reflect.internal.Flags.DEFERRED)
}

object RuntimeReflection extends BasicReflection(scala.reflect.runtime.universe) {

  //for reasons I don't understand, this method is really unhappy if defined in the super class
  def zeroArgMethodsOf[A:scala.reflect.runtime.universe.TypeTag] = {
    val typ = implicitly[scala.reflect.runtime.universe.TypeTag[A]].tpe.asInstanceOf[u.Type]
    zeroArgMethods(typ)
  }

}

