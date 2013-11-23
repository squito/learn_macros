package com.imranrashid.oleander.macros

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 *
 */
class BasicDefMacrosTest extends FunSuite with ShouldMatchers {
  test("call a def macro") {
    BasicDefMacros.hello{7} should be ("hello 7")
    BasicDefMacros.hello{def x = 18} should be ("hello ()") //result doesn't really matter, just showing this works
  }

  test("error check a macro") {
    BasicDefMacros.errorIfNotConstant(17) should be (5)

    import shapeless.test.illTyped
    illTyped(
      """
        BasicDefMacros.errorIfNotConstant{def x = 17}
      """)

  }
}
