package scoogle.tests

import org.scalatest.{BeforeAndAfter, Spec}
import scoogle._
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

class IntegSpecs extends Spec with ShouldMatchers with BeforeAndAfter {
  val input = """
package scala.util
class DynamicVariable[T >: scala.Nothing <: scala.Any] extends java.lang.Object with scala.ScalaObject {
  def this(init : T) = { /* compiled code */ }
  def value : T = { /* compiled code */ }
  def withValue[S >: scala.Nothing <: scala.Any](newval : T)(thunk : => S) : S = { /* compiled code */ }
  def value_=(newval : T) : scala.Unit = { /* compiled code */ }
  override def toString() : scala.Predef.String = { /* compiled code */ }
}
"""

  val expectedFuncs = List(
     Func("DynamicVariable[T]#value", FuncType(Star("DynamicVariable", TParam("T")), TParam("T"))),
     Func("DynamicVariable[T]#withValue[S]", FuncType(Star("DynamicVariable", TParam("T")), TParam("T"), TParam("S"), TParam("S"))),
     Func("DynamicVariable[T]#value_=", FuncType(Star("DynamicVariable", TParam("T")), TParam("T"), Star("scala.Unit"))),
     Func("DynamicVariable[T]#toString", FuncType(Star("DynamicVariable", TParam("T")), Star("scala.Predef.String")))
    )
  var funcs : List[Func] = Nil
  override def beforeAll {
    funcs = Funcs.forClass(ScalapParser.parse(input).get._2)
  }

  describe("do") {
    it("all") {
      funcs should equal(expectedFuncs)
    }
  }
}

// TODO
// Identify thunks and interpret them as Function0[T]
