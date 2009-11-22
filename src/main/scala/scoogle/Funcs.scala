package scoogle

sealed trait Type
case object Unit extends Type
final case class Star(name : String) extends Type
final case class TParam(name : String) extends Type
final case class StarStar(name : String, of : Type) extends Type

case class Func(name : String, funcType : FuncType)
case class FuncType(args : Type*)

object Funcs {

  def funcName(clsSpec : ClassSpec, funcSpec : FuncSpec) = {
    clsSpec.name + (if (clsSpec.typeVars.isEmpty) "" else "[" + clsSpec.typeVars.reduceLeft(_+","+_) + "]") + "#" + funcSpec.name
  }

  def forClass(clsSpec : ClassSpec) : List[Func] = {
    clsSpec.funcSpecs.map { (funcSpec : FuncSpec) =>
      Func(funcName(clsSpec, funcSpec), FuncType(Star(clsSpec.name), Star(funcSpec.resultType)))
    }
  }
}