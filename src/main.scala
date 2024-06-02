package Main

import interprete._
import lexer._
import reductor._
import java.util.Scanner
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]) = {


    var input: String = readLine
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


    //    for (i <- 0 to listaPrueba.length - 1) {
    //      println("Expresion: " + listaPrueba(i))
    //      val ecuacionParseada = leerCalculoLambda(listaPrueba(i))
    //      println("Expresion" + ecuacionParseada)
    //      val expresion = parsear(ecuacionParseada)
    //      println("Parsear: " + expresion)
    //      println("Desparsear " + desparsear(expresion))
    //      val variables = variablesLibres(expresion, List(), List())
    //      println("Libres: " + variables._1 + " Ligadas: " + variables._2)
    //      val sustitucionExpresion = sustitucion(expresion)
    //      println("Sustitucion: " + sustitucionExpresion)
    //    }

    while (input != null) {
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
