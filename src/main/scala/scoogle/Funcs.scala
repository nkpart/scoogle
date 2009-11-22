package scoogle

import java.lang.String

sealed trait Type
case object Unit extends Type
final case class Star(name: String) extends Type
final case class TParam(name: String) extends Type
final case class StarStar(name: String, of: Type) extends Type

case class Func(name: String, funcType: FuncType)
case class FuncType(args: Type*)

object Funcs {
  def funcName(clsSpec: ClassSpec, funcSpec: FuncSpec) = {
    val typeSection = if (clsSpec.typeVars.isEmpty) "" else "[" + clsSpec.typeVars.reduceLeft(_ + "," + _) + "]"
    clsSpec.name + typeSection + "#" + funcSpec.name
  }

  def isTypeVar(clsSpec: ClassSpec, funcSpec: FuncSpec, typeName: String) =
    clsSpec.typeVars.contains(typeName) || funcSpec.typeVars.contains(typeName)

  def forClass(clsSpec: ClassSpec): List[Func] = {
    clsSpec.funcSpecs.map((funcSpec: FuncSpec) => {
      val resultTypeName = funcSpec.resultType
      val resultType = if (isTypeVar(clsSpec, funcSpec, resultTypeName)) TParam(resultTypeName) else Star(resultTypeName)
      Func(funcName(clsSpec, funcSpec), FuncType(Star(clsSpec.name), resultType))
    })
  }
}