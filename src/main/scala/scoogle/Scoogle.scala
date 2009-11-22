package scoogle

import java.io.{InputStreamReader, BufferedReader}

object Scoogle {
  def main(args : Array[String]) {
    val in = new BufferedReader(new InputStreamReader(System.in))
    var contents = ""
    var line : String = in.readLine
    while (line != null) {
      contents += line + "\n"
      line = in.readLine
    }

    val psa = ScalapParser.parse(contents).get._2

    psa.flatMap(Funcs.forClass _).foreach(println _)
  }
}