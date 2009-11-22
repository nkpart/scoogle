package scoogle.tests

import java.lang.String
import org.scalatest.{BeforeAndAfter, Spec}
import scoogle._
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

class FuncsSpecs extends Spec with ShouldMatchers with BeforeAndAfter {
  val simple = ClassSpec(false, "Simple", Nil, List(FuncSpec("toString", Nil, Nil, PType("String"))))
  val simpleWithArg = ClassSpec(false,"Num", Nil, List(FuncSpec("add", Nil, List(("num", PType("Num"))), PType("Num"))))
  val identity = ClassSpec(false, "Identity", List(PType("T")), List(
    FuncSpec("value", Nil, Nil, PType("T")),
    FuncSpec("as", List(PType("S")), List(("s", PType("S"))), PType("Identity", List(PType("S"))))
    //FuncSpec("repeat", Nil, List(("n", "Int")), "List[T]")
    ))


  var simpleFuncs: List[Func] = Nil
  var identityFuncs: List[Func] = Nil
  var simpleArgFuncs: List[Func] = Nil

  override def beforeAll {
    simpleFuncs = Funcs.forClass(simple)
    identityFuncs = Funcs.forClass(identity)
    simpleArgFuncs = Funcs.forClass(simpleWithArg)
  }

  describe("funcs") {
    it("has a func for each funcspec") {
      simpleFuncs.length should equal(simple.funcSpecs.length)
      identityFuncs.length should equal(identity.funcSpecs.length)
    }

    describe("with a simple class") {
      it("gives a good name to each funcspec") {
        simpleFuncs(0).name should equal("Simple#toString")
      }
      it("identifies the return type as the last type") {
        simpleFuncs(0).funcType.args.last should equal(Star("String"))
      }
      it("identifies the class as the first param") {
        simpleFuncs(0).funcType.args.first should equal(Star("Simple"))
      }
    }

    describe("with a class with a function with an arg") {
      it ("identifies the arg") {
        simpleArgFuncs(0).funcType.args should equal(List(Star("Num"), Star("Num"), Star("Num"))) 
      }
    }

    describe("with a typed class") {
      it("gives a good name to each funcspec") {
        identityFuncs(0).name should equal("Identity[T]#value")
      }
      it("identifies a Type-param return value") {
        identityFuncs(0).funcType.args.last should equal(TParam("T"))
      }
      it ("types the class name properly") {
        identityFuncs(0).funcType.args.first should equal(Star("Identity", TParam("T")))
      }

      it("names a func with a type param accordingly") {
        identityFuncs(1).name should equal("Identity[T]#as[S]")
        //Func("Identity[T]#as[S]", FuncType(Star("Identity", TParam("T")), TParam("S")))
      }
      it("identifies a typed arg/result") {
        identityFuncs(1).funcType should equal(FuncType(Star("Identity", TParam("T")), TParam("S"), Star("Identity", TParam("S"))))
      }
    }

    describe("complete ") {
      it("is complete") {
        simpleFuncs should equal(List(Func("Simple#toString", FuncType(Star("Simple"), Star("String")))))
        identityFuncs should equal(List(
          Func("Identity[T]#value", FuncType(Star("Identity", TParam("T")), TParam("T"))),
          Func("Identity[T]#as[S]", FuncType(Star("Identity", TParam("T")), TParam("S"), Star("Identity", TParam("S"))))
          ))
      }
    }
  }
}

class FuncsObjectSpecs extends Spec with ShouldMatchers with BeforeAndAfter {
  val longT: PType = PType("scala.Long")
  val simple = ClassSpec(true, "Simple", Nil, List(FuncSpec("double", Nil, List("a" -> longT, "b" -> longT), longT)))

  var simpleFuncs: List[Func] = Nil

  override def beforeAll {
    simpleFuncs = Funcs.forClass(simple)
  }

  describe("funcs") {
    describe("with a simple object") {
      it("gives a good name to each funcspec") {
        simpleFuncs(0).name should equal("Simple#double")
      }
      it("does not include the object in the type") {
        simpleFuncs(0).funcType.args should equal(List(Star("scala.Long"), Star("scala.Long"), Star("scala.Long")))
      }
    }
  }
}