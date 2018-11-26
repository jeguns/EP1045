# =============================== #
# CAPÍTULO III: PROGRAMACIÓN EN R #
# =============================== #

# El objetivo de este capítulo es que programes tus propias funciones en R.
# Archivo de datos y códigos disponibles en https://github.com/jeguns/EP1045

# EJERCICIO 1

conv.fc = function(F){
  C = 5/9*(F-32)
  return(C)
}
conv.fc(32)

conv.fc = function(F){
  C = 5/9*(F-32)
  print(paste("El valor de",F,"grados Farenheit es equivalente a",C,"grados Celsius"))
}
conv.fc(32)

# EJERCICIO 2

calcula.pf = function(P,IGV,Q){
  PF = P*(1+IGV)*Q
  print(paste("El precio final de",Q,"productos (incluido IGV) es de",PF,"soles"))
}

calcula.pf(10,0.18,3)

# EJERCICIO 3

decide = function(n){
  if(n%%2==0){
    print(paste("El número",n,"es par"))
  }else{
    print(paste("El número",n,"es impar"))
  }
}
decide(19)
decide(26)

# Aplicación de diseño descendente: Uso de una función dentro de una función:

Decide = function(n){
  if(n==0){
    print(paste("Ingrese un valor distinto de cero"))
  }else{decide(n)}
  }
}
Decide(19)
Decide(26)
Decide(0)

# EJERCICIO 4

el.mayor = function(a,b){
  if(a==b){
    print(paste("Los números son iguales"))
  }
  if(a>b){
    print(paste(a,"es mayor que",b))
  }
  if(a<b){
    print(paste(b,"es mayor que",a))
  }
}

el.mayor(3,8)
el.mayor(-9,3.2)
el.mayor(-4,-4)

el.mayoR = function(a,b){
  if(a==b){
    print(paste("Los números son iguales"))
  }else{
    if(a>b){
      print(paste(a,"es mayor que",b))
    }else{
      print(paste(b,"es mayor que",a))
    }
  }
}

el.mayoR(3,8)
el.mayoR(-9,3.2)
el.mayoR(-4,-4)


# EJERCICIO 5
# Cree un programa que lea un número e indique si es múltiplo de 7 o no.

# EJERCICIO 6
# Cree un programa que lea la altura y estatura de una persona y calcule su Índice de Masa Corporal. 
# Luego, basado en dicho índice debe indicar si la persona tiene peso normal (18.5 <= IMC < 25)

# EJERCICIO 7
# Cree un programa que lea un vector de datos e indique si su mediana es al menos 10

# EJERCICIO 8
# Cree un programa que lea un vector de datos y calcule el coeficiente de asimetría:
# AS = 3*(PROMEDIO - MEDIANA)/DESV.ESTÁNDAR

# EJERCICIO 9
# Cree un programa que calcule la velocidad final de un carro que parte con una velocidad inicial "v0"
# conocida (m/s) y acelera a razón de "a" m/s^2 durante "t" segundos. Luego debe determinar si la velocidad
# final sobrepasa los 20 m/s o no.

# EJERCICIO 10
# Cree un programa que sume los primeros n números enteros positivos (1,2,3,4,...), 
# sin utilizar la fórmula S = n(n+1)/2

# EJERCICIO 11
# Cree un programa que sume los primeros n números impares sin utilizar la fórmula: 
# 1+3+5+...+(2n-1)=n^2

# EJERCICIO 12
# Cree un programa que lea un vector de valores e indique cuántos son negativos así 
# como la suma de éstos

# EJERCICIO 13
# Cree un programa que lea un vector de valores e indique cuántos son pares y además
# la suma de los impares

# EJERCICIO 14
# Cree un programa mediante el cual se ingresa un vector con el nombre de n personas
# y se imprima lo siguiente:
# "El nombre de la persona 1 es xxxxx". 
# Luego realizar lo mismo para las demás personas, hasta la última.

# EJERCICIO 15
# Cree un programa que lea un vector de datos y calcule su media geométrica. Utilice
# el comando FOR

MG = function(x){
  P = 1
  n = length(x)
  for(i in 1:n){
    P = P*x[i]
  }
  mg = P^(1/n)
  print(paste("la media geométrica es igual a",mg))
}

# EJERCICIO 16
# Cree un programa que lea un vector de tiempos de espera en una cola de banco (en 
# minutos) e indique el tiempo que debió esperar la "i"-ésima persona. Por ejemplo, 
# si el vector es (3,5,9,10,2,3), la primera persona no esperó nada, la segunda 3 
# minutos, la tercera 3+5=8, la cuarta 3+5+9=17 y así sucesivamente.
# Además debe indicar si la persona espero más de 8 minutos o no.

Espera = function(x){
  S = 0
  for(i in 1:length(x)){
    S = S+x[i]
    print(paste("La persona número",i,"esperó",S,"minutos"))
  }  
}

# EJERCICIO 17
# Cree un programa que lea una fecha y diga si ya pasó o aún no.

fecha = function(x){
  if(x<Sys.Date()){
    print(paste("La fecha",x,"ya pasó"))
  }else{
    print(paste("La fecha",x,"aún no pasó"))
  }
}

fecha("2018-01-12")
fecha("2018-12-12")

# EJERCICIO 18
# Una tienda descuenta 15% a los clientes que consumen más de 500 soles. 
# Determine el monto final que paga una persona que realiza una compra en la tienda

precio = function(x){
  if(x>500){
    pf = 0.85*x
  }else{
    pf = x
  }
  print(paste("El precio final a pagar es de",pf,"soles"))
}

# EJERCICIO 19
# Cree un programa que solo permita ingresar los caracteres "S" o "N".

programa = function(x){
  if((x!='S') && (x!='N')){
    print(paste("Solo puede ingresar los valores S o N"))
  }  
}

# EJERCICIO 20
# Cree un programa que lea 2 números p y q. Luego, debe calcular:
# p^q, si p<q
# p*q, si p>q
# p+q, si p=q

calculo = function(p,q){
  if(p<q){R=p^q}
  if(p>q){R=p*q}
  if(p==q){R=p+q}
  return(R)
}

# EJERCICIO 21
# Cree un programa que permita calcular el factorial de un número

facto = function(x){
  P = 1
  for(i in 1:x){
    P = P*i
  }
  print(paste(x,"!=",P))
}

Facto = function(x){
  P = 1
  for(i in 1:x){
    P = P*i
  }
  return(P)
}


# EJERCICIO 22
# Cree un programa que calcule la combinatoria de n en x.
# Pista: Puede usar el programa del ejercicio 21

comb = function(n,x){
  C = Facto(n)/(Facto(x)*Facto(n-x))
  print(paste("La combinatoria de",n,"en",x,"es igual a",C))
}

Comb = function(n,x){
  C = Facto(n)/(Facto(x)*Facto(n-x))
  return(C)
}

# EJERCICIO 23
# Cree un programa que lea tres números y diga si la multiplicación de los dos 
# primeros es igual al tercero

program = function(a,b,c){
  if(a*b==c){
    print(paste("La multiplicación de",a,"*",b,"es igual a",c))
  }else{
    print(paste("La multiplicación de",a,"*",b,"no es igual a",c))
    
  }
}

# EJERCICIO 24
# La probabilidad de falla de una laptop es igual a 0.02, mientras que la 
# probabilidad de que fallen x laptops de un total de n es igual a
# Comb(n,x)*0.02^x*0.98^(n-x)
# Calcule la probabilidad de que en un total de 20 laptops fallen 4.

binomial = function(n,x){
  Prob = Comb(n,x)*0.02^x*0.98^(n-x)
  print(paste("La probabilidad de que en un total de",n,"laptops fallen",x,"es igual a", Prob))
}

binomial(20,4)

# EJERCICIO 25
# Si durante un minuto entran 2 alumnos por la puerta 1, la probabilidad de que
# entren x alumnos en y minutos es:
# (2*y)^x * e^(-2*y) / x!
# Determine la probabilidad de que ingresen 25 alumnos en un lapso de 10 minutos.

poisson = function(x,y){
  Prob= (2*y)^x*exp(-2*y)/Facto(x)
  print(paste("La probabilidad de que ingresen",x,"alumnos en un lapso de",y,"minutos es igual a",Prob))
}
poisson(25,10)