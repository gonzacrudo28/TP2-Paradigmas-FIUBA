package interprete

import modelo.*

import scala.annotation.tailrec


//casos para probar -> (λx.λx.(y x) z)
//(λf.(f λx.λy.x) ((λx.λy.λf.((f x) y) a) b))
//(λx.λy.x y)
//(λx.λy.y (λx.(x x) λx.(x x)))

//
//def parsear (tokens :List[CalculoLambda]) :CalculoLambda = tokens match {
//  case Nil => NIL()
//  case LAMBDAstr() :: VAR(name) :: tail if tail.nonEmpty => LAMBDA(name,parsear(tail))
//  case DOT() :: tail => parsear(tail)
//  case SPACE() :: tail => parsear(tail)
//  case LPAR() :: tail => APP(parsear(tail.slice(0,1)),parsear(tail.slice(2,tail.indexOf(RPAR()))))
//  case VAR(name) :: tail if tail.nonEmpty => parsear(tail)
//  case VAR(name) :: Nil => VAR(name)
//}

 // (x (x y))

 // hay algunos casos q rompe como: (λx.λy.y (λx.(x x) λx.(x x)))
def verificarParentesisAfuera(tokens: List[CalculoLambda]) : List[CalculoLambda]  = tokens match{
  case x :: xs if x == LPAR() && xs.head ==  LAMBDAstr() && xs.last == RPAR() =>
    tokens.drop(1).dropRight(1)

  case _ => tokens
}

//((λx.λy.(y x)) x)
//(λx.λy.((y x) x))   => λx.λy.((y x) x)
//(x (λx.λy.(y x)))
def parsear2(tokens: List[CalculoLambda]): CalculoLambda = tokens match {
  case Nil => NIL()
  case x :: xs if x == LAMBDAstr() => abstraerExp(tokens)
  case x :: xs if x == LPAR() && xs.head == LAMBDAstr() => abstraerExp(tokens.drop(1).dropRight(1))
  case x :: xs if x == LPAR() => aplicarExp(tokens.drop(1).dropRight(1))
  case VAR(_) :: Nil => tokens.head
  case _ => parsear2(tokens.drop(1))
}

def abstraerExp(lambdas: List[CalculoLambda]): CalculoLambda = lambdas match{
  case LAMBDAstr() :: VAR(name) :: xs  => LAMBDA(name.toString,parsear2(xs.tail) )
  case _ => NIL()
}
//LAMBDA(arg:  , body: CalculoLambda)
//λx.λy.(y x) x -> aplicarExp -> APP(λx.λy.(y x), x)
//(λx.λy.y (λx.(x x) λx.(x x))) -> λx.λy.y (λx.(x x) λx.(x x)) -> Si tengo una VAR seguida de un SPACE -> Analizo los
// parentesis de lo que sigue -> Estan cerrados? -> Si, tengo una APP(Hasta el Space, Space en adelante)

// (λx.λy.y (λx.(x x) λx.(x x)))
def aplicarExp(lambdas: List[CalculoLambda]): CalculoLambda = lambdas match {
  case Nil => NIL()
  case VAR(_) :: Nil => lambdas.head
  case x :: xs if x == LPAR()  =>
    APP(parsear2(lambdas.take(buscarSpace(lambdas))), (parsear2(lambdas.drop(buscarSpace(lambdas)+1))) )
  case _ =>
    APP(parsear2(lambdas.take(buscarSpace(lambdas))), (parsear2(lambdas.drop(buscarSpace(lambdas)+1))) )
}
//List(VAR(y), SPACE(), VAR(x), RPAR(), SPACE(), VAR(x))

@tailrec
def buscarSpace(expresion : List[CalculoLambda], contador : Int = 0):Int =  expresion match {
  case x :: xs if x == SPACE() => contador
  case x :: xs if x == LPAR() => buscarSpace(expresion.drop(lengthExp(expresion)), contador + lengthExp(expresion))
  case _ => buscarSpace(expresion.drop(1), contador + 1)
}

@tailrec
def lengthExp(expression: List[CalculoLambda], contador: Int=0): Int = expression match{
  case x :: xs if x ==RPAR() => contador + 1
  case x :: xs => lengthExp(xs, contador + 1)
  case Nil => contador
}

//APP(f: CalculoLambda, v: CalculoLambda)

//(()) (z)
//λx.λy.(x y)--> λx.λy app x y
//ExpresionLAMBDAexp(x,LAMBDAexp(y,APP(VAR(x),VAR(y))))


//λ -> abs
//( -> abs
//espacio -> abs
// var sola -> var
//Expresion  LAMBDAexp(x,LAMBDAexp(y,APP(VAR(x),VAR(y)))) ACA

// (λx.λy.(y x) x)
/*
  λx.λy.(y x) x

 */

// (λx.λy.(y x) x)
//(λx x)



/*
private def _interpretarEcuacion(ecuacion: List[Operador | Float], paramsActuales: Stack[Expresion]): Expresion = {
  ecuacion match
    case Nil =>
      if (paramsActuales.length > 1) throw Exception("Ecuacion invalida")
      else paramsActuales.head
    case x::xs =>
      x match
        case f: Float =>
          paramsActuales.push(Valor(f))
          _interpretarEcuacion(xs, paramsActuales)
        case op: Operador =>
          op match
            case SUMA =>
              val e1: Expresion = paramsActuales.pop()
              val e2: Expresion = paramsActuales.pop()
              paramsActuales.push(Suma(e1, e2))
            case MULT =>
              val e1: Expresion = paramsActuales.pop()
              val e2: Expresion = paramsActuales.pop()
              paramsActuales.push(Mult(e1, e2))
            case RAIZ =>
              val e: Expresion = paramsActuales.pop()
              paramsActuales.push(Raiz(e))
            case CUADRADO =>
              val e: Expresion = paramsActuales.pop()
              paramsActuales.push(Cuadrado(e))
          _interpretarEcuacion(xs, paramsActuales)
}*/




