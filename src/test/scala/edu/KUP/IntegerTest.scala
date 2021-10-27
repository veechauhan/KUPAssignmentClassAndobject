package edu.KUP

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class IntegerTest extends AnyFlatSpec {
  val zero = Zero
  val one = new Succ(zero)
  val two = new Succ(one)
  val three = new Succ(two)

  "Successor" should "give successor of zero" in {
    assert(zero.successor.isZero == false)
  }
  "Predecessor" should "give predecessor of one " in {
    assert(one.predecessor.isZero == true)
  }
  "Successor  and - " should "give Successor  of 3-2 " in {
    assert(three.-(two).successor.isZero == false)
  }
  "Predecessor and - " should "give predecessor of 3-2 " in {
    assert(three.-(two).predecessor.isZero == true)
  }
  "Successor  and + " should "give Successor  of 1+0 " in {
    assert(one.+(zero).successor.isZero == false)
  }
  "Predecessor and + " should "give predecessor of 1-0 " in {
    assert(one.-(zero).predecessor.isZero == true)
  }
   "If we check zero isZero " should "give true" in {
    assert(zero.isZero)
  }
  "Predecessor of two" should "give one " in {
    assert(two.predecessor==one)
  }


}
