package interprete

import modelo.*

import scala.annotation.tailrec

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

def desparsear(expresion: CalculoLambda) : String = expresion match {
  case VAR(name) => name
  case LAMBDA(name,exp) => "λ" + name + "." + desparsear(exp)
  case APP(exp1,exp2) => "(" +    desparsear(exp1)  + " " + desparsear(exp2) + ")"
  case _ => ""
}
// APP(VAR(y),VAR(x))
// LAMBDA(x,APP(VAR(y),VAR(x)))   
// LAMBDA(x,APP(APP(VAR(y),VAR(x)),VAR(w))) 
def desparsearAST(arbol: String): String ={
  val arbolSplit = arbol.split("APP")
  print(arbolSplit)
  arbol



}