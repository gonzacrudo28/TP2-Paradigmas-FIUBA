package Main

import interprete._
import lexer._
import java.util.Scanner
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]) = {
    var input: String = readLine

    while (input != null) {
      val ecuacionParseada = leerCalculoLambda(input)
      println("Expresion" + ecuacionParseada)
      val expresion = parsear2(ecuacionParseada)
      println("pARSER:  " + expresion)
      //val resultado = procesar(expresion)
      //printf("Resultado = %f \n", resultado)

      input = readLine
    }
  }
}