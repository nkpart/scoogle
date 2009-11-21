import java.lang.String
import org.scalatest.{BeforeAndAfter, Spec}
import scoogle._
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

class FuncsSpecs extends Spec with ShouldMatchers with BeforeAndAfter {
  val simple = ClassSpec("Simple", Nil, List(FuncSpec("toString", Nil, Nil, "String")))
  var simpleFuncs: List[Func] = Nil

  override def beforeAll {
    simpleFuncs = Funcs.forClass(simple)
  }

  describe("funcs") {
    describe("with a simple class") {

      it("should have a func for each funcspec") {
        simpleFuncs.length should equal(simple.funcSpecs.length)
      }
      it("should give a good name to each funcspec") {
        val f = simpleFuncs(0)
        f.name should be("Simple#toString")
      }
      it("should identify the return type as the last type") {
        val f = simpleFuncs(0)
        f.funcType.args.last should be(Star("String"))
      }
    }
    //    it("should interpret a simple class") {
    //      val funcs = Funcs.forClass(simple)
    //      forClass.should equal (List(Func("Simple#toString", FuncType(Star("Simple"), Star("String")))))
    //    }
  }
}