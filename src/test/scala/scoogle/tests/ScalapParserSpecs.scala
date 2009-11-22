package scoogle.tests

import java.lang.String
import scoogle._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

class ScalapParserSpecs extends Spec with ShouldMatchers {
  def check[U](p: ScalapParser.Parser[U], input: String, expected: U) {
    val pr = ScalapParser.parse(p, input)

    if (!pr.successful) println(pr)
    pr.successful should equal(true)
    pr.get should equal(expected)
  }

  describe("parser") {
    val sp = ScalapParser
    it("has a bunch of little parsers that work") {
      check(ScalapParser.dotwords, "a.b.C", "a.b.C")
      check(ScalapParser.typevar_p, "T <: Any", PType("T"))
      check(ScalapParser.typevar_p, "+T <: Any", PType("T"))
      check(ScalapParser.typevars_p, "[-T, S for Some { type A }]", List(PType("T"), PType("S")))
      check(ScalapParser.typevars_p, "[T[_]]", List(PType("T[_]")))
      check(ScalapParser.typevars_p, "[+T1]", List(PType("T1")))
      check(ScalapParser.func_name_p, "value_=", "value_=")
      check(ScalapParser.func_name_p, "$init$", "$init$")
      check(ScalapParser.valuetype_p, "scala.List[A]", PType("scala.List", List(PType("A"))))
      check(ScalapParser.valuetype_p, "T", PType("T"))
      check(ScalapParser.package_p, "package foo", "foo")

      check(ScalapParser.func_p, "def this(init : T) = { /* compiled code */ }", None)
      check(ScalapParser.func_p, "def value : T = { /* compiled code */ }",
        Some(FuncSpec("value", Nil, Nil, PType("T"))))
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
    it ("parses a class dump") {
      val parsed: Option[(String, ClassSpec)] = sp.parse(dynamicVariable)
      parsed.isDefined should equal(true)
    }

    it("correctly matches package") {
      val parsed = sp.parse(dynamicVariable)
      val packageName: String = parsed.get._1
      packageName should equal("scala.util")
    }

    it("pulls out class name and type variables") {
      val parsed: Option[(String, ClassSpec)] = sp.parse(dynamicVariable)
      val classSpec: ClassSpec = parsed.get._2
      classSpec.name should equal("DynamicVariable")
      classSpec.typeVars should equal(List(PType("T")))
    }

    it("correctly matches functions in the class") {
      val parsed: Option[(String, ClassSpec)] = sp.parse(dynamicVariable)
      val funcs = parsed.toList flatMap (_._2.funcSpecs)
      val value_ = FuncSpec("value", Nil, Nil, PType("T"))
      val withValue_ = FuncSpec("withValue", List(PType("S")), List(("newval", PType("T")), ("thunk", PType("S"))), PType("S"))
      funcs.contains(value_) should equal(true)
      funcs.contains(withValue_) should equal(true)
    }
  }

  describe ("given a trait") {

    val function1 = """
package scala
trait Function1[-T1 >: scala.Nothing <: scala.Any, +R >: scala.Nothing <: scala.Any] extends java.lang.Object with scala.ScalaObject {
  def $init$() : scala.Unit = { /* compiled code */ }
  def apply(v1 : T1) : R
  override def toString() : java.lang.String = { /* compiled code */ }
  def compose[A >: scala.Nothing <: scala.Any](g : scala.Function1[A, T1]) : scala.Function1[A, R] = { /* compiled code */ }
  def andThen[A >: scala.Nothing <: scala.Any](g : scala.Function1[R, A]) : scala.Function1[T1, A] = { /* compiled code */ }
}
    """

    it ("parses function1") {
      val parsed : Option[(String, ClassSpec)] = ScalapParser.parse(function1)
      parsed.isDefined should equal(true)
    }

    it ("knows some stuff about function1's trait decl") {
      val spec : ClassSpec = ScalapParser.parse(function1).get._2
      spec.name should equal("Function1")
      spec.typeVars should equal(List(PType("T1"), PType("R")))
    }

    it ("knows some stuff about function1's methods") {
      val spec : ClassSpec = ScalapParser.parse(function1).get._2
      def f1Type(a : String, b : String) : Star = Star("Function1", TParam(a), TParam(b))
      val baseF = f1Type("T1", "R")
      spec.funcSpecs should equal(List(
        FuncSpec("$init$", Nil, Nil, PType("scala.Unit")),
        FuncSpec("apply", Nil, List(("v1", PType("T1"))), PType("R")),
        FuncSpec("toString", Nil, Nil, PType("java.lang.String")),
        FuncSpec("compose", List(PType("A")), List(("g", PType("scala.Function1", List(PType("A"),PType("T1"))))), PType("scala.Function1", List(PType("A"),PType("R")))),
        FuncSpec("andThen", List(PType("A")), List(("g", PType("scala.Function1", List(PType("R"),PType("A"))))), PType("scala.Function1", List(PType("T1"),PType("A"))))
        ))
    }
  }
}
