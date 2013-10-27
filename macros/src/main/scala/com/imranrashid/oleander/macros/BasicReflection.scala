package com.imranrashid.oleander.macros

/**
 *
 */
object BasicReflection {

  def zeroArgMethods(typ: reflect.runtime.universe.Type) = {
    typ.members.collect{
      case m if m.isMethod => m.asMethod
    }.filter{_.paramss.isEmpty}
  }


  def zeroArgMethodsOf[A:reflect.runtime.universe.TypeTag] = {
    val typ = implicitly[reflect.runtime.universe.TypeTag[A]].tpe
    zeroArgMethods(typ)
  }

  def undefinedZeroArgMethodsOf[A: reflect.runtime.universe.TypeTag] = {
    zeroArgMethodsOf[A].filter{isDefined(_)}
  }

  def targetMethodsOf[A: reflect.runtime.universe.TypeTag] = {
    undefinedZeroArgMethodsOf[A].filter{m =>
      val rt = m.returnType
      (rt =:= reflect.runtime.universe.typeOf[Int]) || (rt =:= reflect.runtime.universe.typeOf[Float])
    }
  }



  def isDefined(method: reflect.runtime.universe.MethodSymbol): Boolean = {
    //from http://stackoverflow.com/questions/16792824/test-whether-a-method-is-defined
    isDeferred(method)
  }

  def isDeferred(sym: reflect.runtime.universe.MethodSymbol) = sym
    .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    .hasFlag(scala.reflect.internal.Flags.DEFERRED)
}
