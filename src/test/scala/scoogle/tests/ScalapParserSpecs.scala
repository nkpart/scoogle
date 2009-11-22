package scoogle.tests

import java.lang.String
import scoogle._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

class ScalapParserSpecs extends Spec with ShouldMatchers {
  def check[U](p: ScalapParser.Parser[U], input: String, expected: U) {
    val pr = ScalapParser.parse(p, input)

    pr.successful should be(true)

    pr.get should be(expected)
  }

  describe("parser") {
    val sp = ScalapParser
    it("has a bunch of little parsers that work") {
      check(ScalapParser.dotwords, "a.b.C", "a.b.C")
      check(ScalapParser.typevar_p, "T <: Any", "T")
      check(ScalapParser.typevars_p, "[T, S for Some { type A }]", List("T", "S"))
      check(ScalapParser.typevars_p, "[T[_]]", List("T[_]"))
      check(ScalapParser.package_p, "package foo", "foo")
    }

    val dynamicVariable = """
package scala.util
class DynamicVariable[T >: scala.Nothing <: scala.Any] extends java.lang.Object with scala.ScalaObject {
  def this(init : T) = { /* compiled code */ }
  def value : T = { /* compiled code */ }
  def withValue[S >: scala.Nothing <: scala.Any](newval : T)(thunk : => S) : S = { /* compiled code */ }
  def value_=(newval : T) : scala.Unit = { /* compiled code */ }
  override def toString() : scala.Predef.String = { /* compiled code */ }
}
"""

    it("parses a class dump") {
      val parsed: Option[(String, ClassSpec)] = sp.parse(dynamicVariable)
      parsed.isDefined should be(true)
    }

    it("correctly matches package") {
      val parsed = sp.parse(dynamicVariable)
      val packageName : String = parsed.get._1
      packageName should be("scala.util")
    }

    it ("pulls out class name and type variables") {
      val parsed: Option[(String, ClassSpec)] = sp.parse(dynamicVariable)
      val classSpec : ClassSpec = parsed.get._2
      classSpec.name should equal("DynamicVariable")
      classSpec.typeVars should equal(List("T"))
    }

    it("correctly matches functions in the class") {
      val parsed: Option[(String, ClassSpec)] = sp.parse(dynamicVariable)
      val funcs = parsed.toList flatMap (_._2.funcSpecs)
      val value_ = FuncSpec("value", Nil, Nil, "T")
      val withValue_ = FuncSpec("withValue", List("S"), List(("newval", "T"), ("thunk", "S")), "S")
      funcs.contains(value_) should be(true)
      funcs.contains(withValue_) should be(true)
    }
  }
}
