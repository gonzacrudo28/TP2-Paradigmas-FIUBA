package interprete

import modelo.*

import scala.annotation.tailrec
import scala.collection.mutable.Stack

//PARSER LISTO, LA UNICA FORMA DE QUE ROMPA ES QUE NO HAYA UNA EXPRESION VALIDA
def parsear(tokens: List[CalculoLambda]): CalculoLambda = tokens match {
  case Nil => NIL()
  case x :: xs if x == LAMBDAstr() => abstraerExp(tokens)
  case x :: xs if x == LPAR() => aplicarExp(tokens.drop(1).dropRight(1))
  case VAR(_) :: Nil => tokens.head
  case _ => parsear(tokens.drop(1))
}
def abstraerExp(lambdas: List[CalculoLambda]): CalculoLambda = lambdas match{
  case LAMBDAstr() :: VAR(name) :: xs  => LAMBDA(name.toString,parsear(xs.tail) )
  case _ => NIL()
}
def aplicarExp(lambdas: List[CalculoLambda]): CalculoLambda = lambdas match {
  //case Nil => NIL()
  case VAR(_) :: Nil => lambdas.head
  case x :: xs if x == LPAR() && xs.head == LPAR() =>
    APP(parsear(lambdas.take(buscarSpaceConParentesis(lambdas))), (parsear(lambdas.drop(buscarSpaceConParentesis(lambdas)+1))))
  case x :: xs if x == LPAR()  =>
    APP(parsear(lambdas.take(buscarSpaceConParentesis(lambdas))), (parsear(lambdas.drop(buscarSpaceConParentesis(lambdas)+1))))
  case _ =>
    APP(parsear(lambdas.take(buscarSpaceConParentesis(lambdas))), (parsear(lambdas.drop(buscarSpaceConParentesis(lambdas)+1))) )
}
@tailrec
def buscarSpaceConParentesis(expresion : List[CalculoLambda], contadorEspacio : Int = 0,contadorLPAR: Int = 0, contadorRPAR: Int = 0):Int =  expresion match {
  case x :: xs if x == LPAR() =>
    buscarSpaceConParentesis(expresion.drop(1), contadorEspacio+ 1, contadorLPAR + 1, contadorRPAR)
  case x :: xs if x == RPAR() =>
    buscarSpaceConParentesis(expresion.drop(1), contadorEspacio+ 1, contadorLPAR, contadorRPAR + 1)
  case x :: xs if x == SPACE() && contadorLPAR == contadorRPAR => contadorEspacio
  case _ => buscarSpaceConParentesis(expresion.drop(1), contadorEspacio + 1, contadorLPAR, contadorRPAR)
}
def desparsear(expresion: String) :  String ={
  val tokens = tokenizarDesparcer(expresion)
  val tokenParceado = parsear(tokens)
  val expresionDesparceada = desparsearExpresion(tokenParceado)
  expresionDesparceada
}
def desparsearExpresion(expresion: CalculoLambda) : String = expresion match {
  case VAR(name) => name
  case LAMBDA(name,exp) => "λ" + name + "." + desparsearExpresion(exp)
  case APP(exp1,exp2) => "(" +    desparsearExpresion(exp1)  + " " + desparsearExpresion(exp2) + ")"
  case _ => ""
}
def tokenizarDesparcer (expresion: String,ultimaFuncion: Stack[String] = Stack[String]()): List[CalculoLambda] = expresion.toLowerCase.toList match {
  case 'l' :: 'a' :: 'm' :: 'b' :: 'd' :: 'a' :: '(' :: x :: ',' :: tail => LAMBDAstr() ::VAR(x.toString) :: DOT()  :: tokenizarDesparcer(tail.mkString,ultimaFuncion.push("lambda"))
  case 'a' :: 'p' :: 'p' :: '(' ::  tail =>  LPAR() :: tokenizarDesparcer(tail.mkString,ultimaFuncion.push("app"))
  case 'v' :: 'a' :: 'r' :: '(' :: x :: ')' :: tail => VAR(x.toString)  :: tokenizarDesparcer(tail.mkString,ultimaFuncion)
  case ',' :: tail => SPACE() :: tokenizarDesparcer(tail.mkString,ultimaFuncion)
  case '(' :: tail => LPAR() :: tokenizarDesparcer(tail.mkString,ultimaFuncion)
  case ')' :: tail if ultimaFuncion.isEmpty => RPAR() :: tokenizarDesparcer(tail.mkString,ultimaFuncion)
  case ')' :: tail if  ultimaFuncion.top == "app" && ultimaFuncion.pop() == "app"  => RPAR() ::tokenizarDesparcer(tail.mkString,ultimaFuncion)
  case ')' :: tail if  ultimaFuncion.top == "lambda" && ultimaFuncion.pop() == "lambda"  => tokenizarDesparcer(tail.mkString,ultimaFuncion)
  case _ :: tail => tokenizarDesparcer(tail.mkString,ultimaFuncion)
  case Nil => List()
}