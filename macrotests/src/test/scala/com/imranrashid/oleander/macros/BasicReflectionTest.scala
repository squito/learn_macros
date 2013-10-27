package com.imranrashid.oleander.macros

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BasicReflectionTest extends FunSuite with ShouldMatchers {
  test("zeroArgMethods") {
    val methodNames = BasicReflection.zeroArgMethodsOf[BasicTrait].map{_.name.toString}.toSet
    methodNames should be (Set("x","y","z", "asInstanceOf", "isInstanceOf", "q", "m"))
  }

  test("zeroArg undefined methods") {
    val methodNames = BasicReflection.undefinedZeroArgMethodsOf[BasicTrait].map{_.name.toString}.toSet
    methodNames should be (Set("x","z","q", "m"))
  }

  test("zeroArg undefined Int & Float methods") {
    val methodNames = BasicReflection.targetMethodsOf[BasicTrait].map{_.name.toString}.toSet
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
