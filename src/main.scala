package Main

import interprete.*
import lexer.*
import reductor.*

import java.util.Scanner
import Estados.*
import modelo.CalculoLambda

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]) = {
    val input: String = readLine("Ingrese su expresion, AST o nueva configuracion: ")
    leerInput(input, CBN)
  }

  @tailrec
  def leerInput(input: String, estado: Estados): Unit = {
    val texto = ("Ingrese su expresion o setear nueva configuracion: ")
    input match {
      case "exit" =>
        System.exit(0)
      case "set free-variables" =>
        println("Se seteo el estado a Free Variables")
        leerInput(readLine(texto), FV)

      case "set call-by-value" =>
        println("Se seteo el estado a Call by Value")
        leerInput(readLine(texto), CBV)

      case "set call-by-name" =>
        println("Se seteo el estado a Call by Name")
        leerInput(readLine(texto), CBN)

      case x if x.contains("APP") || x.contains("LAMBDA") || x.contains("VAR") =>
        println("Expresion del AST: " + desparsear(x))
        leerInput(readLine(texto), estado)
      case _ =>
        val expresionParseada = conversionAlfa(parsear(leerCalculoLambda(input)))
        estado match {
          case FV =>
            val variables = variablesLibres(expresionParseada, List(), List())
            println("Libres: " + variables._1)
            leerInput(readLine(texto), FV)
          case CBV =>
            println("Expresion reducida por CALL BY VALUE: " + desparsearExpresion(reductorCallByValue(expresionParseada)))
            leerInput(readLine(texto), CBV)
          case CBN =>
            println("Expresion reducida por CALL BY NAME: " + reductorCallByName(expresionParseada))
            leerInput(readLine(texto), CBN)
        }

    }
    }
}




