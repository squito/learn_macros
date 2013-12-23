package com.imranrashid.oleander.macros

import org.scalatest.{Matchers, FunSuite}
import java.nio.ByteBuffer

class ByteBufferBackedTest extends FunSuite with Matchers {
  test("immutable") {
    @ByteBufferBacked[BBTest] class Blah(bb: ByteBuffer)
    val bb = ByteBuffer.allocate(80)
    val b = new Blah(bb)
    bb.putInt(7)
    bb.putFloat(5.4f)
    bb.putInt(3)

    b.x should be (7)
    b.y should be (5.4f)
    b.z should be (3)
  }

  test("mutable") {
    @MutableByteBufferBacked[BBTest] class Foo(bb: ByteBuffer)
    val bb = ByteBuffer.allocate(80)
    val b = new Foo(bb)
    b.x = 19
    b.y = -2.3f
    b.z = 32

    b.x should be (19)
    b.y should be (-2.3f)
    b.z should be (32)

  }
}


trait BBTest {
  def x: Int
  def y: Float
  def z: Int
}

