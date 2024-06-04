package reductor

import modelo._


def conversionAlfa(expresion: CalculoLambda): CalculoLambda = {
  val expLigadas = sustitucion(expresion)
  val (libres, ligadas) = variablesLibres(expLigadas, List(), List())
  val hashLibres = libres.groupBy(x => x).filter(_._2.size > 1).map((k, v) => (k, v.length))
  println(hashLibres)
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
  case APP(exp1, exp2) => //Si mando los dos a la vez el hash no se me actualiza
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

// (λx.x y)-> APP(LAMBDA(x,VAR(x)),VAR(y))
// ->VAR(y)
//(λy.(x y) w) -> APP(LAMBDA(y,APP(VAR(x),VAR(y), VAR(w) )
// -> APP(x w)
//(λw.λx.(y x) z) -> APP(LAMBDA(w,LAMBDA(x,APP(VAR(y),VAR(x)))),VAR(z))
//(λw.λx.((y x) w) z) -> APP(LAMBDA(w,LAMBDA(x,APP(APP(VAR(y),VAR(x)),VAR(w)))),VAR(z))
def reductorCallByName(expresion: CalculoLambda): CalculoLambda = expresion match {
  case APP(exp1 , exp2) => reducirCallByName(exp1,exp2)
}

// exp1:LAMBDA(y,APP(VAR(x),VAR(y))    exp2:VAR(w)
//exp1:LAMBDA(w,LAMBDA(x,APP(VAR(y),VAR(x))))    exp2:VAR(z) 
//exp1: LAMBDA(w,LAMBDA(x,APP(APP(VAR(y),VAR(x)),VAR(w)))))  exp2: VAR(z))
def reducirCallByName(exp1 :CalculoLambda,exp2: CalculoLambda) : CalculoLambda = exp1 match {
  case LAMBDA(variable,expAbs) if expReducibleCBN(variable, expAbs) => reducirCBN(variable,expAbs,exp2)
  case LAMBDA(variable,expAbs) => expAbs
  case VAR(_) if exp2 == VAR =>APP(exp1,exp2)
}

// variableAbs : y     exp: APP(VAR(y),VAR(x))
// variableAbs: w    exp: LAMBDA(x,APP(VAR(y),VAR(x)))    sale de (λw.λx.(y x) z)
// variableAbs: w    exp: LAMBDA(x,APP(APP(VAR(y),VAR(x)),VAR(w))) sale de (λw.λx.((y x) w) z)
def expReducibleCBN(variableAbs : String, exp: CalculoLambda) : Boolean = exp match{
  case APP(e1,e2) => expReducibleCBN(variableAbs, e1) || expReducibleCBN(variableAbs,e2)
  case VAR(name) => name == variableAbs
  case LAMBDA(variable2,APP(f, v))  => expReducibleCBN(variableAbs, v)
  case _ => false
}
//if f == APP
// y   APP(VAR(x),VAR(y))    VAR(w) 
// APP entre: y   VAR(x)   VAR(w)  ||  y  VAR(y)   VAR(w)
// variable:w  exp1:LAMBDA(x,APP(VAR(y),VAR(x)))  exp2:VAR(z)  sale de (λw.λx.(y x) z)
// variable:w  exp1: LAMBDA(x,APP(APP(VAR(y),VAR(x)),VAR(w))))  exp2:VAR(z)  sale de (λw.λx.((y x) w) z)
def reducirCBN(variable : String, exp1 : CalculoLambda , exp2 : CalculoLambda) : CalculoLambda = exp1 match{
  case VAR(name) if name == variable => exp2
  case VAR(name) => VAR(name)
  case APP(app1, app2) =>  APP(reducirCBN(variable, app1, exp2),reducirCBN(variable, app2, exp2))
  case LAMBDA(variable2,APP(exp3,exp4))=> APP(exp3,reducirCBN(variable, exp4, exp2))
}
 

/*def reductorCallByName(expresion: CalculoLambda) = expresion match {
  case APP(exp1, exp2) => reemplazarCBN(exp1, exp2)
}
*/
//def reemplazarCBN(exp1: CalculoLambda, exp2: CalculoLambda) = exp1 match{
//  case LAMBDA(name, body) => reducirBody(name, body, exp2)
//}
//
//def reducirBody(name: String)