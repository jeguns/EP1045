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
# Cree un programa que lea un vector de tiempos de espera en una cola de banco (en minutos) e indique el 
# tiempo que debió esperar la "i"-ésima persona. Por ejemplo, si el vector es (3,5,9,10,2,3), la primera 
# persona no esperó nada, la segunda 3 minutos, la tercera 3+5=8, la cuarta 3+5+9=17 y así sucesivamente.
# Además debe indicar si la persona espero más de 8 minutos o no.

# EJERCICIO 11
# Cree un programa que lea una fecha y diga si ya pasó o aún no.

  