package reductor

import modelo._
import interprete._


def conversionAlfa(expresion: CalculoLambda): CalculoLambda = {
  val expLigadas = sustitucion(expresion)
  val (libres, ligadas) = variablesLibres(expLigadas, List(), List())
  val hashLibres = libres.groupBy(x => x).filter(_._2.size > 1).map((k, v) => (k, v.length))
  libresSust(expLigadas, hashLibres)
}

def variablesLibres(expresion: CalculoLambda, libres: List[String], ligadas: List[String]): (List[String], List[String]) = expresion match {
  case LAMBDA(name, body) =>
    val nLigadas = if (!ligadas.contains(name)) ligadas :+ name else ligadas
    variablesLibres(body, libres, nLigadas)
  case VAR(name) if !ligadas.contains(name) && !libres.contains(name) => (libres :+ name, ligadas)
  case VAR(name) => (libres, ligadas)
  case APP(exp1, exp2) =>
    val (libres1, ligadas1) = variablesLibres(exp1, libres, ligadas)
    val (libres2, ligadas2) = variablesLibres(exp2, libres, ligadas)
    ((libres1.distinct ++ libres2.distinct), (ligadas1 ++ ligadas2).distinct)
}

def sustitucion(expresion: CalculoLambda): CalculoLambda = {
  val (libres, ligadas) = variablesLibres(expresion, List(), List())
  cambiarRepetidas(expresion, libres, ligadas)
}


def cambiarRepetidas(lambda: CalculoLambda, libres: List[String], ligadas: List[String]): CalculoLambda = lambda match {
  case LAMBDA(name, body) if libres.contains(name) && ligadas.contains(name) =>
    val renombre = name + "*"
    LAMBDA(renombre, cambiarRepetidas(cambiarNombre(body, name, renombre), libres, ligadas :+ renombre))
  case LAMBDA(name, body) => LAMBDA(name, cambiarRepetidas(body, libres, ligadas :+ name))
  case VAR(name) => VAR(name)
  case APP(exp1, exp2) => APP(cambiarRepetidas(exp1, libres, ligadas), cambiarRepetidas(exp2, libres, ligadas))
}

def cambiarNombre(lambda: CalculoLambda, viejo: String, original: String): CalculoLambda = lambda match {
  case VAR(name) if name == viejo => VAR(original)
  case VAR(name) => VAR(name)
  case LAMBDA(name, body) => LAMBDA(name, cambiarNombre(body, viejo, original))
  case APP(exp1, exp2) => APP(cambiarNombre(exp1, viejo, original), cambiarNombre(exp2, viejo, original))
}


def libresSust(expresion: CalculoLambda, hashLibres: Map[String, Int]): CalculoLambda = expresion match {
  case LAMBDA(name, body) => LAMBDA(name, libresSust(body, hashLibres))
  case VAR(name) if hashLibres.getOrElse(name, 0) >= 2 =>
    val cant = hashLibres.getOrElse(name, 0)
    val updatedMap = hashLibres.updated(name, cant - 1)
    libresSust(VAR(name + "'" * cant), updatedMap)
  case VAR(name) => VAR(name)
  case APP(exp1, exp2) => 
    val nExp = reemplazarExp(exp1, hashLibres)
    val hashLibres1 = actualizoHash(exp1, hashLibres)
    APP(nExp, libresSust(exp2, hashLibres1))
}

def reemplazarExp(exp: CalculoLambda, hashLibres: Map[String, Int]): CalculoLambda = exp match {
  case VAR(name) if hashLibres.getOrElse(name, 0) >= 2 =>
    val cant = hashLibres.getOrElse(name, 0)
    VAR(name + "'" * cant)
  case _ => exp
}

def actualizoHash(exp: CalculoLambda, hashLibres: Map[String, Int]): Map[String, Int] = exp match {
  case VAR(name) if hashLibres.getOrElse(name, 0) >= 2 =>
    val cant = hashLibres.getOrElse(name, 0)
    hashLibres.updated(name, cant - 1)
  case _ => hashLibres
}

def reductorCallByName(expresion: CalculoLambda): String = {
  val r1 = wrapperReductorCallByName(expresion)
  val r2 = wrapperReductorCallByName(r1)

  r2 match {
    case _ if r1 == r2 => desparsearExpresion(r1)
    case _ => reductorCallByName(r2)
  }
}

def wrapperReductorCallByName(expresion: CalculoLambda): CalculoLambda = expresion match {
  case APP(exp1, exp2) => reducirCallByName(exp1, exp2)
  case LAMBDA(arg, body) => LAMBDA(arg, wrapperReductorCallByName(body))
  case VAR(name) => expresion
}

def reducirCallByName(exp1: CalculoLambda, exp2: CalculoLambda): CalculoLambda = exp1 match {
  case LAMBDA(variable, expAbs) => convertirExp(variable, exp2, expAbs)
  case APP(a1, a2) => APP(reducirCallByName(a1, a2), exp2)
  case VAR(a) => APP(exp1, wrapperReductorCallByName(exp2))
}

def convertirExp(variable: String, param: CalculoLambda, exp: CalculoLambda): CalculoLambda = exp match {
  case VAR(name) if name == variable => param
  case VAR(name) => exp
  case APP(a1, a2) =>  wrapperReductorCallByName(APP(convertirExp(variable,param,a1), convertirExp(variable,param,a2)))
  case LAMBDA(variable2,expAbs) if variable2 == variable => exp
  case LAMBDA(variable2, expAbs) => LAMBDA(variable2, convertirExp(variable, param, expAbs))
}

def reductorCallByValue(expresion: CalculoLambda): CalculoLambda = expresion match{
  case APP(e1, e2) =>
    val e1reducida = reductorCallByValue(e1)
    val e2reducida = reductorCallByValue(e2)
    e1reducida match {
      case  LAMBDA(arg, body) => reductorCallByValue(sustituir(body, arg, e2reducida))
      case _ => APP(e1reducida, e2reducida)
    }
  case _ => expresion
}

def sustituir(body: CalculoLambda, arg: String, sustituto: CalculoLambda): CalculoLambda = body match {
  case VAR(name) if name == arg => sustituto
  case VAR(name) if name != arg => VAR(name)
  case LAMBDA(a, b) if a != arg => LAMBDA(a, sustituir(b, arg, sustituto))
  case LAMBDA(a, b) if a == arg => LAMBDA(a, b)
  case APP(e1, e2) => APP(sustituir(e1, arg, sustituto), sustituir(e2, arg, sustituto))
  case other => other
}

