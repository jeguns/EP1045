
# ==================================== #
# CAPÍTULO II: ESTADÍSTICA DESCRIPTIVA #
# ==================================== #

# Vamos a utilizar el archivo de datos propinas.txt
# Archivo de datos y códigos disponibles en https://github.com/jeguns/EP1045

# ------------------------------------------------ #
# Tablas de frecuencia para variables cualitativas #
# ------------------------------------------------ #

# Lectura de datos
propinas = read.table("propinas.txt",T)

# Estructura de datos
str(propinas)

# Reordenamiento de categorías
propinas$dia   = factor(propinas$dia,levels(propinas$dia)[c(2,4,3,1)])

# Asignación de variables
attach(propinas)

# Construcción de la tabla usando paquete stats
f     = table(dia)              # Frecuencias absolutas
fr    = prop.table(table(dia))  # Frecuencias relativas
p     = fr*100                  # Frecuencias porcentuales
tabla = cbind(f,fr,p)           # Tabla de frecuencias
tabla

# Construcci?n de la tabla usando paquete dplyr
library(dplyr)
tabla = propinas %>% 
  count(dia) %>%
  mutate(fr = round(prop.table(n),3)) %>%
  mutate(p  = fr*100) 
tabla
tabla %>% summarise_at(c("n","fr", "p"), sum)

# Actividad: construya las tablas de frecuencia correspondientes para las variables:
# Momento del d?a (orden de las categor?as: d?a, noche)
# Nivel de satisfacci?n (orden de las categor?as: 5,4,3,2,1)

# ----------------------------------------------------------- #
# Tablas de frecuencia para variables cuantitativas discretas #
# ----------------------------------------------------------- #

# Construcción de la tabla usando paquete stats
f     = table(cantidad)             # Frecuencias absolutas
fr    = prop.table(table(cantidad)) # Frecuencias relativas
p     = fr*100                      # Frecuencias porcentuales
F     = cumsum(f)                   # Frecuencias absolutas acumuladas
Fr    = cumsum(fr)                  # Frecuencias relativas acumuladas
P     = cumsum(p)                   # Frecuencias porcentuales acumuladas
tabla = cbind(f,fr,p,F,Fr,P)        # Tabla de frecuencias
tabla

# Construcción de la tabla usando paquete dplyr
library(dplyr)
tabla = propinas %>% 
  count(cantidad) %>%
  mutate(fr = prop.table(n)) %>%
  mutate(p  = fr*100) %>%
  mutate(F  = cumsum(n)) %>%
  mutate(Fr = cumsum(fr)) %>%
  mutate(P  = cumsum(p))
tabla
tabla %>% summarise_at(c("n","fr", "p"), sum)

# ----------------------------------------------------------- #
# Tablas de frecuencia para variables cuantitativas continuas #
# ----------------------------------------------------------- #

library(grDevices)
r    = diff(range(total))
r
k    = nclass.Sturges(total)
k    # redondeo simple
tic  = r/k
tic  # redondeo por exceso
ndec = 2
tic  = ceiling((tic*10^ndec))/10^ndec
tic
#tic = 5.31
min(total)
tic

cortes    = min(total) + tic*seq(0,9)
cortes[1] = min(total) - 1e-6

propinas %>% 
  mutate(category=cut(total, breaks=cortes)) %>%
  count(category) %>%
  mutate(fr = prop.table(n)) %>%
  mutate(p  = fr*100) %>%
  mutate(F  = cumsum(n)) %>%
  mutate(Fr = cumsum(fr)) %>%
  mutate(P  = cumsum(p))

# Actividad: construya la tabla de frecuencia correspondiente para la variable propina
