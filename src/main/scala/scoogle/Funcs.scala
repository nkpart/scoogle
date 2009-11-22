package scoogle

import java.lang.String

sealed trait Type
case object Unit extends Type
final case class Star(name: String, params : Type*) extends Type
final case class TParam(name: String) extends Type

case class Func(name: String, funcType: FuncType)
case class FuncType(args: Type*)

trait Typeable[T] {
  def typeValue(t : T) : Type
}
object Typeables {
  implicit def classSpec : Typeable[ClassSpec] = new Typeable[ClassSpec] {
    def typeValue(cs : ClassSpec) : Type = Star(cs.name, cs.typeVars map (TParam(_)):_*)
  }
}

object Funcs {
  import Typeables._
  def typeOf[T](t : T)(implicit typeable: Typeable[T]) : Type = typeable.typeValue(t)

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
      Func(funcName(clsSpec, funcSpec), FuncType(typeOf(clsSpec), resultType))
    })
  }
}