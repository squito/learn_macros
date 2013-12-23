package com.imranrashid.oleander.macros

import org.scalatest.{Matchers, FunSuite}

class MacrosWithReflectionTest extends FunSuite with Matchers {
  test("get zero arg methods") {
    @FillDefsWithReflection[MyTrait] class Blah {
      val a = 16
    }

    val b = new Blah()
    b.a should be (16)
    b.x should be (5)
    b.y should be (39.3f)
    b.z should be (7.0f)
  }
}


trait MyTrait {
  def x: Int
  def y: Float = 39.3f
  def z: Float
}

