package com.imranrashid.oleander.macros

import org.scalatest.{Matchers, FunSuite}

class SimpleTraitFillInTest extends FunSuite with Matchers{

  test("fill in trait defs") {
    //the trait is already declared, but the macro actually supplies the defs
    @FillTraitDefs class Foo extends SimpleTrait {}
    val y = new Foo()
    y.x should be (5)
    y.y should be (7.0f)
  }

  test("add trait as super"){
    //we need to both add in the trait as a super, and also fill in the defs
    @AddTraitAsSuper class Blah {}
    val x = new Blah()
    x.isInstanceOf[SimpleTrait] should be (true)
    x.x should be (5)
    x.y should be (7.0f)
  }

  test("add trait with quasiquotes"){
    @QuasiQuoteAddTrait class Ooga {}
    val z = new Ooga()
    z.isInstanceOf[SimpleTrait] should be (true)
    z.x should be (5)
    z.y should be (7.0f)

    //make sure we keep original defs also

    @QuasiQuoteAddTrait class Wakka {
      def q = "hi there"
    }
    val w = new Wakka()
    w.x should be (5)
    w.y should be (7.0f)
    w.q should be ("hi there")


    //and make sure we keep any other traits we define
    @QuasiQuoteAddTrait class Blargh extends SomeTrait {
      def b = "ooga"
    }
    val aBlargh = new Blargh()
    aBlargh.isInstanceOf[SomeTrait] should be (true)
    aBlargh.isInstanceOf[SimpleTrait] should be (true)
    aBlargh.a should be (8)
    aBlargh.b should be ("ooga")
    aBlargh.x should be (5)
    aBlargh.y should be (7.0f)
  }
}


trait SomeTrait {
  def a = 8
}
