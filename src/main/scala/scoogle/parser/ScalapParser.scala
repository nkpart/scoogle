package scoogle

import scala.util.parsing.combinator._

case class FuncSpec(name : String, typeVars : List[String], args : List[(String,String)], resultType : String)

case class ClassSpec(name : String, typeVars : List[String], funcSpecs : List[FuncSpec])

trait MoreParsers {
  self : RegexParsers =>

  def flat[T](p : Parser[Option[List[T]]]) = p ^^ { _ match {
      case Some(x) => x
      case None => Nil
    }
  }

  // TODO semigroup?
  def plus(p : Parser[~[String, String]]) = p ^^ { case (a ~ b) => a + b }

  def sum(p : Parser[List[String]], sep : String) = p ^^ { x => x.reduceLeft(_+sep+_)}

  def upto(s : String) = ("[^" + s + "]*").r <~ s
}

object ScalapParser extends JavaTokenParsers with MoreParsers {
  
  // Example!
  val ex = """
package scala.util
class DynamicVariable[T >: scala.Nothing <: scala.Any] extends java.lang.Object with scala.ScalaObject {
  def this(init : T) = { /* compiled code */ }
  def value : T = { /* compiled code */ }
  def withValue[S >: scala.Nothing <: scala.Any](newval : T)(thunk : => S) : S = { /* compiled code */ }
  def value_=(newval : T) : scala.Unit = { /* compiled code */ }
  override def toString() : scala.Predef.String = { /* compiled code */ }
}
"""

  def dotwords = repsep(ident, ".") ^^ { x => x.reduceLeft(_+"."+_) }
  ///TODO keep around type vars to avoid double parsing later
  def valuetype_p : Parser[String] = plus(dotwords ~ (sum(typevars_p, ",") ^^ { "[" + _ + "]" } | ""))
  def typevar_p : Parser[String] = """[\-\+]?""".r ~> """[A-Z]+(\[\_\])?""".r <~ """[^,^\]]*""".r
  def typevars_p : Parser[List[String]] = "[" ~> repsep(typevar_p, ",") <~ "]"
  def package_p : Parser[String] = "package" ~> dotwords
  def func_type_p = ":" ~> valuetype_p
  def func_args_p = rep("(" ~> repsep(ident ~ ":" ~ opt("=>") ~ valuetype_p ^^ {case (name ~ _ ~ _ ~ value) => (name, value)},",") <~ ")").map { x => x.flatMap(e => e)}
  def func_name_p : Parser[String] = """\$?[\w=]+\$?""".r
  
  def func_p  = opt("override" | "implicit") ~ "def" ~> func_name_p ~ flat(opt(typevars_p)) ~ func_args_p ~ opt(func_type_p) <~ opt("""= \{.*\}""".r) ^^ { case (name ~ oT ~ args ~ oR) => {
      oR map { r => FuncSpec(name, oT, args, r) }
    }
  }
  
  def class_p = ("class" | "trait") ~> ident ~ flat(opt(typevars_p)) ~ upto("{") ~ (rep(func_p) ^^ {x => x.flatMap(e=>e)}) <~ "}" ^^ {
    case (name ~ typeVars ~ extendsbits ~ funcSpecs) => ClassSpec(name, typeVars, funcSpecs)
  }
  
  def whole = package_p ~ class_p ^^ { case (a ~ b) => a -> b }
  
  def parse(s : String) : Option[(String, ClassSpec)] = {
    val pr = parse(whole, s)
    println(pr)
    if (pr.successful) Some(pr.get) else None
  }
  
}
