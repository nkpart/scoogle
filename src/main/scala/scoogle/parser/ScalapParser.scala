package scoogle

import scala.util.parsing.combinator._

case class PType(name : String, ts : List[PType])
object PType { def apply(n : String) : PType = apply(n, Nil) }

case class FuncSpec(name : String, typeVars : List[PType], args : List[(String,PType)], resultType : PType)

case class ClassSpec(isObject : Boolean, name : String, typeVars : List[PType], funcSpecs : List[FuncSpec])

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
  def typevar_p : Parser[PType] = """[\-\+]?""".r ~> ("""[\w\.]+(\[\_\])?""".r ~ (typevars_p | success(Nil)) ^^ {
    case (name ~ bits) => PType(name, bits)
  }) <~ """[^,^\]]*""".r
  def typevars_p : Parser[List[PType]] = "[" ~> repsep(typevar_p, ",") <~ "]"
  def package_p : Parser[String] = "package" ~> dotwords
  def func_type_p = ":" ~> valuetype_p
  def maybeRepeats_p = (valuetype_p ~ """\*""".r ^^ { case (pt ~ star) => PType("scala.Array", List(pt)) }) | valuetype_p
  def func_args_p = rep("(" ~ opt("implicit") ~> repsep(ident ~ ":" ~ opt("=>") ~ maybeRepeats_p ^^ {case (name ~ _ ~ _ ~ value) => (name, value)},",") <~ ")").map { x => x.flatMap(e => e)}
  def func_name_p : Parser[String] = """\$?[\w=\+\*\?!_\-\:><]+\$?""".r

  def func_p : Parser[Option[FuncSpec]]  = rep("override" | "implicit" | "protected") ~ ("def" | "val" | "var") ~> func_name_p ~ flat(opt(typevars_p)) ~ func_args_p ~ opt(func_type_p) <~ opt("""= \{.*\}""".r) ^^ { case (name ~ oT ~ args ~ oR) => {
      oR map { r => FuncSpec(name, oT, args, r) }
    }
  }

  def inner_object_p = "object" ~ ident ~ upto("{") ~ rep(func_p) ~ "}" ~> success[Option[FuncSpec]](None)
  def inner_class_p = opt("final") ~ "class" ~ ident ~ upto("{") ~ rep(func_p) ~ "}" ~> success[Option[FuncSpec]](None)

  def t : Parser[Option[FuncSpec]] = (inner_object_p | inner_class_p)

  def inside_p : Parser[List[FuncSpec]] = (rep(func_p | t) ^^ {(x : List[Option[FuncSpec]]) => x.flatMap(e=>e)})

  def class_p = rep("class" | "trait" | "abstract" | "sealed" | "final") ~> opt("object") ~ ident ~ flat(opt(typevars_p)) ~ upto("{") ~
          inside_p <~
    "}" ^^ {
    case (obj ~ name ~ typeVars ~ extendsbits ~ funcSpecs) => ClassSpec(obj.isDefined, name, typeVars, funcSpecs)
  }

  def whole = package_p ~ rep(class_p) ^^ { case (a ~ b) => a -> b }

  def parse(s : String) : Option[(String, List[ClassSpec])] = {
    val pr = parse(whole, s)
    if (pr.successful) Some(pr.get) else None
  }

  def parse_!(s : String) : List[ClassSpec] = parse(s).get._2
  def parse1_!(s : String) : ClassSpec = parse_!(s)(0)
}
