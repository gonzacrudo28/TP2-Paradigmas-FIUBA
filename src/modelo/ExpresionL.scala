package modelo

sealed trait CalculoLambda

case class VAR(name: String) extends CalculoLambda

case class LAMBDA(arg: String, body: CalculoLambda) extends CalculoLambda

case class LAMBDAstr() extends CalculoLambda

case class DOT() extends CalculoLambda

case class SPACE() extends CalculoLambda

case class LPAR() extends CalculoLambda

case class RPAR() extends CalculoLambda


case class APP(f: CalculoLambda, v: CalculoLambda) extends CalculoLambda

case class NIL() extends CalculoLambda