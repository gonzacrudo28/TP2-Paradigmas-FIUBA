package reductor

import modelo._

//("VARIABLES" + variablesLibres1(LAMBDA("f", LAMBDA("x", APP(APP(VAR("y"), VAR("y")), VAR("z"))))))
//ME FALTA EL CASO DONDE HAY VARIABLE DOS VECES UNA LIBRE Y OTRA LIGADA

val libres = List()
val ligadas = List()

//println("REPETIDOS λx.x -> " + conversionAlfa(LAMBDA("x",VAR("x"))))
//println("REPETIDOS λx.λx.x -> " + conversionAlfa(LAMBDA("x",LAMBDA("x",VAR("x")))))
//println("x(λz.(x (λw.((w z) y)))) ------->" + conversionAlfa(APP(VAR("x"),LAMBDA("z",APP(VAR("x"),LAMBDA("w",APP(APP(VAR("w"),VAR("z")),VAR("y"))))))))
//println("(λy.(x y) y) ------->" + conversionAlfa(APP(LAMBDA("y", APP(VAR("x"), VAR("y"))), VAR("y"))))

def variablesLibres(expresion: CalculoLambda, libres: List[String], ligadas: List[String]): (List[String], List[String]) = expresion match {
  case LAMBDA(name, body) =>
    val nLigadas = if (!ligadas.contains(name)) ligadas :+ name else ligadas
    variablesLibres(body, libres, nLigadas)
  case VAR(name) if !ligadas.contains(name) && !libres.contains(name) => (libres :+ name, ligadas)
  case VAR(name) => (libres, ligadas)
  case APP(exp1, exp2) =>
    val (libres1, ligadas1) = variablesLibres(exp1, libres, ligadas)
    val (libres2, ligadas2) = variablesLibres(exp2, libres, ligadas)
    ((libres1 ++ libres2).distinct, (ligadas1 ++ ligadas2).distinct)
}

def sustitucion(expresion: CalculoLambda): CalculoLambda = {
  val (libres, ligadas) = variablesLibres(expresion, List(), List())
  cambiarRepetidas(expresion, libres, ligadas)
}


def repetidos(expresion: CalculoLambda, ligadas: List[String]): List[String] = expresion match {
  case LAMBDA(name, body) => repetidos(body, ligadas)
  case VAR(name) if !ligadas.contains(name) => List(name)
  case VAR(name) => List()
  case APP(exp1, exp2) => repetidos(exp1, ligadas) ++ repetidos(exp2, ligadas)
}

def cambiarRepetidas(lambda: CalculoLambda, libres: List[String], ligadas: List[String]): CalculoLambda = lambda match {
  case LAMBDA(name, body) if libres.contains(name) && ligadas.contains(name) =>
    val renombre = name + "'"
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


//SI TENGO UNA APP Y DE CADA LADO TENGO VARIABLES REPETIDAS ENTONCES TENGO QUE RENOMBRARLAS

def conversionAlfa(expresion: CalculoLambda): CalculoLambda = {
  val (libres, ligadas) = variablesLibres(expresion, List(), List())
  val expSinLigadas = cambiarRepetidas(expresion, libres, ligadas)
  val repCant = repetidos(expSinLigadas, ligadas).groupBy(x => x).filter(_._2.size > 1).map((k, v) => (k, v.length))
  sustitucionAlfa(expSinLigadas, repCant, 0)
}

def sustitucionAlfa(expresion: CalculoLambda, repetidas: Map[String, Int], contador: Int): CalculoLambda = {
  expresion match {
    case VAR(name) if repetidas.getOrElse(name, 0) > 1 =>
      val nuevaVar = crearVar(name, contador)
      val nuevasRepetidas = actualizarRepetidas(repetidas, name)
      sustitucionAlfa(VAR(nuevaVar), nuevasRepetidas, contador)
    case APP(f, v) =>
      val nuevoContador = incrementarContador(contador)
      APP(sustitucionAlfa(f, repetidas, nuevoContador), sustitucionAlfa(v, repetidas, nuevoContador))
    case VAR(name) => VAR(name)
    case LAMBDA(name, body) =>
      LAMBDA(name, sustitucionAlfa(body, repetidas, contador))
  }
}
def crearVar(name: String, contador: Int): String = {
  name + "*" * contador
}

def incrementarContador(contador: Int): Int = {
  contador + 1
}

def actualizarRepetidas(repetidas: Map[String, Int], name: String): Map[String, Int] = {
  val cantidad = repetidas.getOrElse(name, 0)
  cantidad match
    case 0 => repetidas
    case 1 => repetidas - name
    case _ => repetidas.updated(name, cantidad - 1)
}

/*def reductorCallByName(expresion: CalculoLambda) = expresion match {
  case APP(exp1, exp2) => reemplazarCBN(exp1, exp2)
}*/

//def reemplazarCBN(exp1: CalculoLambda, exp2: CalculoLambda) = exp1 match{
//  case LAMBDA(name, body) => reducirBody(name, body, exp2)
//}
//
//def reducirBody(name: String)
