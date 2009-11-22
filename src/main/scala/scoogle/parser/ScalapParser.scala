package scoogle

import scala.util.parsing.combinator._

case class PType(name : String, ts : List[PType])
object PType {
  def apply(n : String) : PType = apply(n, Nil)
}

case class FuncSpec(name : String, typeVars : List[PType], args : List[(String,PType)], resultType : PType)

case class ClassSpec(name : String, typeVars : List[PType], funcSpecs : List[FuncSpec])

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

object ScalapParser extends JavaTokenParsers with MoreParsers with ImplicitConversions {

  def dotwords = repsep(ident, ".") ^^ { x => x.reduceLeft(_+"."+_) }
  def valuetype_p : Parser[PType] = dotwords ~ flat(opt(typevars_p)) ^^ flatten2(PType.apply _)
  def typevar_p : Parser[PType] = """[\-\+]?""".r ~> ("""\w+(\[\_\])?""".r ~ (typevars_p | success(Nil)) ^^ {
    case (name ~ bits) => PType(name, bits)
  }) <~ """[^,^\]]*""".r
  def typevars_p : Parser[List[PType]] = "[" ~> repsep(typevar_p, ",") <~ "]"
  def package_p : Parser[String] = "package" ~> dotwords
  def func_type_p = ":" ~> valuetype_p
  def func_args_p = rep("(" ~> repsep(ident ~ ":" ~ opt("=>") ~ valuetype_p ^^ {case (name ~ _ ~ _ ~ value) => (name, value)},",") <~ ")").map { x => x.flatMap(e => e)}
  def func_name_p : Parser[String] = """\$?[\w=]+\$?""".r

  def func_p : Parser[Option[FuncSpec]]  = opt("override" | "implicit") ~ "def" ~> func_name_p ~ flat(opt(typevars_p)) ~ func_args_p ~ opt(func_type_p) <~ opt("""= \{.*\}""".r) ^^ { case (name ~ oT ~ args ~ oR) => {
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
