package com.imranrashid.oleander.macros

import org.scalatest.{Matchers, FunSuite}

class BasicReflectionTest extends FunSuite with Matchers {

  /* for some reason I don't understand, if I try to get the type of the trait through an implicit
     TypeTag *and* my reflection util class takes the universe as a parameter, then I get strange runtime errors, like
     this:

[info] - zeroArgMethods *** FAILED ***
[info]   scala.reflect.internal.MissingRequirementError: class com.imranrashid.oleander.macros.BasicTrait in JavaMirror with java.net.URLClassLoader@23d256fa of type class java.net.URLClassLoader with classpath [...] not found.
[info]   at scala.reflect.internal.MissingRequirementError$.signal(MissingRequirementError.scala:16)
[info]   at scala.reflect.internal.MissingRequirementError$.notFound(MissingRequirementError.scala:17)
[info]   at scala.reflect.internal.Mirrors$RootsBase.getModuleOrClass(Mirrors.scala:48)
[info]   at scala.reflect.internal.Mirrors$RootsBase.getModuleOrClass(Mirrors.scala:61)
[info]   at scala.reflect.internal.Mirrors$RootsBase.staticModuleOrClass(Mirrors.scala:72)
[info]   at scala.reflect.internal.Mirrors$RootsBase.staticClass(Mirrors.scala:119)
[info]   at scala.reflect.internal.Mirrors$RootsBase.staticClass(Mirrors.scala:21)
[info]   at com.imranrashid.oleander.macros.BasicReflectionTest$$anonfun$1$$typecreator1$1.apply(BasicReflectionTest.scala:9)
[info]   at scala.reflect.api.TypeTags$WeakTypeTagImpl.tpe$lzycompute(TypeTags.scala:231)
[info]   at scala.reflect.api.TypeTags$WeakTypeTagImpl.tpe(TypeTags.scala:231)
[info]   ...

      but it works if I remove the implicit from the equation, by getting the type, and using asInstanceOf
      to get around the path-dependent type
   */
  val traitType = scala.reflect.runtime.universe.typeOf[BasicTrait].asInstanceOf[RuntimeReflection.u.Type]

  test("zeroArgMethods") {
    val methodNames = RuntimeReflection.zeroArgMethods(traitType).map{_.name.toString}.toSet
    methodNames should be (Set("x","y","z", "asInstanceOf", "isInstanceOf", "q", "m"))
  }

  test("zeroArg undefined methods") {
    val methodNames = RuntimeReflection.undefinedZeroArgMethods(traitType).map{_.name.toString}.toSet
    methodNames should be (Set("x","z","q", "m"))
  }

  test("zeroArg undefined Int & Float methods") {
    val methodNames = RuntimeReflection.targetMethods(traitType).map{_.name.toString}.toSet
    methodNames should be (Set("x", "q", "m"))
  }

}


trait BasicTrait {
  def x: Int
  def y: Float = 7.0f
  def z: String
  def blah(anArg:Int): Int

  def q: Float
  def m: Int
}
