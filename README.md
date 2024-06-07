<h1>TP SCALA - Grupo Las Maquinas</h1>


### Instrucciones desde la terminal:
<ol>
  <li>
      
    git clone git@github.com:ngraeff/TP2paradigmas.git
    
  </li>
  <li>      

    cd TP2paradigmas

 </li> 
 <li>      

    java -jar out/artifacts/TpScala_jar/TpScala.jar
 </li> 
</ol>

### Comandos del programa:

  exit   --> Salir
  
  set free-variables  --> Setear para free variables
  
  set call-by-value  --> Setear para call-by-value
  
  set call-by-name  --> Setear para call-by-name

### Ingresos del programa:

  Se puede ingresar:
  
  Comandos
  
  Ecuaciones Lambda --> Por ejemplo : (λx.λy.x y)
  
  AST --> Por ejemplo : APP(VAR(x),VAR(y))    

### Consideraciones de uso:

  Si se trabaja en windows, puede fallar el funcionamiento desde la terminal ya que esta misma detecta los λ como ?. Por ejemplo si ingreso --> (λx.λy.x y) se detecta: (?x.?y.x y).




