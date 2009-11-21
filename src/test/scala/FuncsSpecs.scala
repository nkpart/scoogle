import java.lang.String
import scoogle._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

class FuncsSpecs extends Spec with ShouldMatchers {
  describe("funcs") {
    val simple = ClassSpec("Simple", Nil, List(FuncSpec("toString", Nil, Nil, "String")))

    describe("with a simple class") {
      it("should have a func for each funcspec") {
        val funcs = Funcs.forClass(simple)
        funcs.length should equal(simple.funcSpecs.length)
      }
//      it ("should give a good name to each funcspec") {
//
//      }
    }
//    it("should interpret a simple class") {
//      val funcs = Funcs.forClass(simple)
//      forClass.should equal (List(Func("Simple#toString", FuncType(Star("Simple"), Star("String")))))
//    }
  }
}