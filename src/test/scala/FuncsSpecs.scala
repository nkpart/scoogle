import java.lang.String
import org.scalatest.{BeforeAndAfter, Spec}
import scoogle._
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

class FuncsSpecs extends Spec with ShouldMatchers with BeforeAndAfter {
  val simple = ClassSpec("Simple", Nil, List(FuncSpec("toString", Nil, Nil, "String")))
  val identity = ClassSpec("Identity", List("T"), List(FuncSpec("value", Nil, Nil, "T")))
  var simpleFuncs: List[Func] = Nil
  var identityFuncs: List[Func] = Nil

  override def beforeAll {
    simpleFuncs = Funcs.forClass(simple)
    identityFuncs = Funcs.forClass(identity)
  }


  describe("funcs") {
    it("should have a func for each funcspec") {
      simpleFuncs.length should equal(simple.funcSpecs.length)
      identityFuncs.length should equal(identity.funcSpecs.length)
    }

    describe("with a simple class") {
      it("should give a good name to each funcspec") {
        simpleFuncs(0).name should be("Simple#toString")
      }
      it("should identify the return type as the last type") {
        simpleFuncs(0).funcType.args.last should be(Star("String"))
      }
      it("should identify the class as the first param") {
        simpleFuncs(0).funcType.args.first should be(Star("Simple"))
      }
    }

    describe("with a typed class") {
      it("should give a good name to each funcspec") {
        identityFuncs(0).name should be("Identity[T]#value")
      }
      it("should identify a Type-param return value") {
        identityFuncs(0).funcType.args.last should be(TParam("T"))
      }
    }

    describe("complete ") {
      ignore("is complete") {
        simpleFuncs should be(List(Func("Simple#toString", FuncType(Star("Simple"), Star("String")))))
        identityFuncs should be(List(Func("Identity[T]#value", FuncType(StarStar("Identity", TParam("T")), TParam("T")))))
      }
    }
  }
}