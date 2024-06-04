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
    print("Ingrese su expresion o setear nueva configuracion: ")
    var input: String = readLine
    val estado = CBN
    val listaPrueba = List[String](
      "λx.((x y) λz.(x z))",
      "(x λz.(x λw.((w z) y)))",
      "(λx.λy.y (λx.(x x) λx.(x x)))",
      "λy.((x y) y)",
      "(λy.(x y) y)",
      "λx.x",
      "λx.λx.x",
      "λf.λx.(f y)",
      "λf.λx.((y x) z)",
      "λx.λy.λf.((((f x) y) a) b)",
      "(λx.λy.(x y) (y x))",
      "((λx.λy.(x y) (y x)) (x y))")


    var expresion = ""
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
      val expresion = parsear(ecuacionParseada)
      println(expresion)
      val variables = variablesLibres(expresion, List(), List())
      println("Libres: " + variables._1 + " Ligadas: " + variables._2)
      val sustitucionExpresion = conversionAlfa(expresion)
      println("Sustitucion: " + desparsear(sustitucionExpresion))
      val variables1 = variablesLibres(sustitucionExpresion, List(), List())
      if (variables1 != variables){
        println("Nuevas libres: " + variables1._1 + " Nuevas ligadas: " + variables1._2)
      }
      //println("reductor CBN: " + reductorCallByName(sustitucionExpresion))
      print("Ingrese su expresion o setear nueva configuracion: ")
      input = readLine
    }}}

