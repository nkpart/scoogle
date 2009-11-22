package scoogle

import scalaz.Scalaz._
import java.io.{InputStreamReader, BufferedReader}

object Scoogle {
  def s(ft : Type) : String = ft match {
    case TParam(t) => t
    case Star(t, Nil) => t
    case Star(t, xs) => t + "[" + xs.map(s _).mkString(",") + "]"
  }

  def show(f : Func) : String = {
    val pad = Math.max(40 - f.name.size, 0)
    " "*pad + f.name + " = " + f.funcType.args.map(e => s(e)).mkString(" -> ")
  }

  def main(args : Array[String]) {
    val in = new BufferedReader(new InputStreamReader(System.in))
    var contents = ""
    var line : String = in.readLine
    while (line != null) {
      contents += line + "\n"
      line = in.readLine
    }

    val psa = ScalapParser.parse(contents).get._2

    psa.flatMap(Funcs.forClass _).foreach(p => println(show(p)))
  }
}