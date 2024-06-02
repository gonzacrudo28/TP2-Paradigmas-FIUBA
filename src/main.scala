package Main

import interprete.*
import lexer.*
import reductor.*

import java.util.Scanner
import Estados.*
import modelo.CalculoLambda

import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]) = {
    var input: String = readLine
    val estado = CBN
    val listaPrueba = List[String](
      "λx.((x y) λz.(x z))",
      "(x λz.(x λw.((w z) y)))",
      "λy.((x y) y)",
      "λx.x",
      "λx.λx.x",
      "λx.λx.λx.x",
      "λx.λx.λx.λx.x",
      "λf.λx.(f y)",
      "λf.λx.((y x) z)",
      "(λy.(x y) y)",
      "λx.λy.λf.((((f x) y) a) b)",
      "(λx.λy.(x y) (y x))",
      "((λx.λy.(x y) (y x)) (x y))")
    //     val listaConversion = List[String](
    //       ""
    //     )

    var expresion = ""
    //var expresionParseada = CalculoLambda()
    while (input != "exit") {
 //     input match {
//        case "set free-variables" => estado = FV
//        case "set call-by-value" => estado = CBV
//        case "set call-by-name" => estado = CBN
//        case x if x.contains('λ') =>
//          expresionParseada = parsear(leerCalculoLambda(input))
//          println("Expresion: " + expresionParseada)
//        case _ =>
          //println("Expresion: " + desparsear(CalculoLambda(input)))
//      }

      val ecuacionParseada = leerCalculoLambda(input)
      println("Expresion" + ecuacionParseada)
      val expresion = parsear(ecuacionParseada)
      println("Parsear:  " + expresion)
      println("Desparsear " + desparsear(expresion))
      val variables = variablesLibres(expresion, List(), List())
      println("Libres: " + variables._1 + " Ligadas: " + variables._2)
      val sustitucionExpresion = sustitucion(expresion)
      println("Sustitucion: " + sustitucionExpresion)
      println("Desparsear Sustitucion: " + desparsear(sustitucionExpresion))
      val variables1 = variablesLibres(sustitucionExpresion, List(), List())
      println("Libres: " + variables1._1 + " Ligadas: " + variables1._2)

      //      val resultado = procesar(expresion)
      //      printf("Resultado = %f \n", resultado)

      input = readLine
    }
}
}
