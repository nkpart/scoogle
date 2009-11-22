package scoogle

import java.lang.String

sealed trait Type
final case class Star(name: String, params : List[Type]) extends Type
final case class TParam(name: String) extends Type

object Star {
  def apply(name : String) : Star = apply(name, List())
  def apply(name : String, args : Type*) : Star = apply(name, List(args :_*))
}

case class Func(name: String, funcType: FuncType)
case class FuncType(args: Type*)

trait Typeable[T] {
  def typeValue(t : T) : Type
}
object Typeables {
  implicit def classSpec : Typeable[ClassSpec] = new Typeable[ClassSpec] {
    def typeValue(cs : ClassSpec) : Type = Star(cs.name, cs.typeVars map (TParam(_)))
  }

  implicit def thing : Typeable[(List[String], String)] = new Typeable[(List[String], String)] {
    def typeValue(xs : (List[String], String)) : Type = xs match { case (typeVars, name) =>
      if (typeVars.contains(name)) TParam(name) else Star(name)
    }
  }
}

object Funcs {
  import Typeables._
  def typeOf[T](t : T)(implicit typeable: Typeable[T]) : Type = typeable.typeValue(t)

  def typeSection(typevars : List[String]) = if (typevars.isEmpty) "" else "[" + typevars.reduceLeft(_ + "," + _) + "]"
  
  def funcName(clsSpec: ClassSpec, funcSpec: FuncSpec) =
    clsSpec.name + typeSection(clsSpec.typeVars) + "#" + funcSpec.name + typeSection(funcSpec.typeVars)

  def isTypeVar(typeVars: List[String], typeName: String) = typeVars.contains(typeName)

  def forClass(clsSpec: ClassSpec): List[Func] = {
    clsSpec.funcSpecs.map((funcSpec: FuncSpec) => {
      val resultTypeName = funcSpec.resultType
      val typeVars: List[String] = clsSpec.typeVars ++ funcSpec.typeVars
      val resultType = typeOf((typeVars, resultTypeName))
      val parts = typeOf(clsSpec) :: funcSpec.args.map(e => typeOf(typeVars -> e._2)) ::: List(resultType)
      Func(funcName(clsSpec, funcSpec), FuncType(parts : _*))
    })
  }
}