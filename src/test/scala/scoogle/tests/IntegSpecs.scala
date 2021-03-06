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
// DynamicVariable[T] -> T
  val expectedFuncs = List(
     Func("DynamicVariable[T]#value", FuncType(Star("DynamicVariable", TParam("T")), TParam("T"))),
     Func("DynamicVariable[T]#withValue[S]", FuncType(Star("DynamicVariable", TParam("T")), TParam("T"), TParam("S"), TParam("S"))),
     Func("DynamicVariable[T]#value_=", FuncType(Star("DynamicVariable", TParam("T")), TParam("T"), Star("scala.Unit"))),
     Func("DynamicVariable[T]#toString", FuncType(Star("DynamicVariable", TParam("T")), Star("scala.Predef.String")))
    )
  var funcs : List[Func] = Nil
  override def beforeAll {
    funcs = Funcs.forClass(ScalapParser.parse1_!(input))
  }

  describe("do") {
    it("all") {
      funcs should equal(expectedFuncs)
    }
  }
}

class IntegFunction1Specs extends Spec with ShouldMatchers with BeforeAndAfter {
  val input = """
package scala
trait Function1[-T1 >: scala.Nothing <: scala.Any, +R >: scala.Nothing <: scala.Any] extends java.lang.Object with scala.ScalaObject {
  def $init$() : scala.Unit = { /* compiled code */ }
  def apply(v1 : T1) : R
  override def toString() : java.lang.String = { /* compiled code */ }
  def compose[A >: scala.Nothing <: scala.Any](g : scala.Function1[A, T1]) : scala.Function1[A, R] = { /* compiled code */ }
  def andThen[A >: scala.Nothing <: scala.Any](g : scala.Function1[R, A]) : scala.Function1[T1, A] = { /* compiled code */ }
}
    """

  val baseF = Star("Function1", TParam("T1"), TParam("R"))
  def f1Type(a : String, b : String) = Star("scala.Function1", TParam(a), TParam(b))
  val expectedFuncs = List(
     Func("Function1[T1,R]#$init$", FuncType(baseF, Star("scala.Unit"))),
     Func("Function1[T1,R]#apply", FuncType(baseF, TParam("T1"), TParam("R"))),
     Func("Function1[T1,R]#toString", FuncType(baseF, Star("java.lang.String"))),
     Func("Function1[T1,R]#compose[A]", FuncType(baseF, f1Type("A", "T1"), f1Type("A", "R"))),
     Func("Function1[T1,R]#andThen[A]", FuncType(baseF, f1Type("R", "A"), f1Type("T1", "A")))
    )

  var funcs : List[Func] = Nil
  override def beforeAll {
    funcs = Funcs.forClass(ScalapParser.parse1_!(input))
  }

  describe("do") {
    it("all") {
      funcs should equal(expectedFuncs)
    }
  }
}

class IntegFuturesSpec extends Spec with ShouldMatchers with BeforeAndAfter {
  val input = """
package scala.actors
object Futures extends java.lang.Object with scala.ScalaObject {
  def this() = { /* compiled code */ }
  def future[T >: scala.Nothing <: scala.Any](body : => T) : scala.actors.Future[T] = { /* compiled code */ }
  def alarm(t : scala.Long) : scala.actors.Future[scala.Nothing] = { /* compiled code */ }
  def awaitEither[a >: scala.Nothing <: scala.Any, b >: scala.Nothing <: scala.Any](ft1 : scala.actors.Future[a], ft2 : scala.actors.Future[b]) : scala.Any = { /* compiled code */ }
  def awaitAll(timeout : scala.Long, fts : scala.actors.Future[scala.Any]*) : scala.List[scala.Option[scala.Any]] = { /* compiled code */ }
  def fromInputChannel[T >: scala.Nothing <: scala.Any](inputChannel : scala.actors.InputChannel[T]) : scala.actors.Future[T] = { /* compiled code */ }
}
    """

  val expectedFuncs = List(
     Func("Futures#future[T]", FuncType(TParam("T"), Star("scala.actors.Future", TParam("T")))),
     Func("Futures#alarm", FuncType(Star("scala.Long"), Star("scala.actors.Future", Star("scala.Nothing")))),
     Func("Futures#awaitEither[a,b]", FuncType(
       Star("scala.actors.Future", TParam("a")), Star("scala.actors.Future", TParam("b")), Star("scala.Any"))),
     Func("Futures#awaitAll", FuncType(
         Star("scala.Long"),
         Star("scala.Array", Star("scala.actors.Future", Star("scala.Any"))),
         Star("scala.List", Star("scala.Option", Star("scala.Any"))))),
     Func("Futures#fromInputChannel[T]", FuncType(
        Star("scala.actors.InputChannel", TParam("T")),
        Star("scala.actors.Future", TParam("T"))
       ))
    )

  var funcs : List[Func] = Nil
  override def beforeAll {
    funcs = Funcs.forClass(ScalapParser.parse1_!(input))
  }

  describe("do") {
    it("all") {
      funcs should equal(expectedFuncs)
    }
  }
}




// TODO
// Identify thunks and interpret them as Function0[T]
