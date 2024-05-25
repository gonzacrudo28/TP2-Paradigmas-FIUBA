package reductor

import modelo._


//("VARIABLES" + variablesLibres1(LAMBDA("f", LAMBDA("x", APP(APP(VAR("y"), VAR("y")), VAR("z"))))))
//ME FALTA EL CASO DONDE HAY VARIABLE DOS VECES UNA LIBRE Y OTRA LIGADA


def variablesLibres(expresion: CalculoLambda, libres: List[String], ligadas: List[String]): (List[String], List[String]) = expresion match {
  case LAMBDA(name, body) =>
    val nLigadas = ligadas :+ name
    variablesLibres(body, libres, nLigadas)
  case VAR(name) if !ligadas.contains(name) && !libres.contains(name) => (libres :+ name, ligadas)
  case VAR(name) => (libres, ligadas)
  case APP(exp1, exp2) => val (libres1, ligadas1) = variablesLibres(exp1, libres, ligadas)
    variablesLibres(exp2, libres1, ligadas1)
}
