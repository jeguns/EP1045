
# ==================================== #
# CAP脥TULO II: ESTAD脥STICA DESCRIPTIVA #
# ==================================== #

# Vamos a utilizar el archivo de datos propinas.txt
# Archivo de datos y c贸digos disponibles en https://github.com/jeguns/EP1045

# ------------------------------------------------ #
# Tablas de frecuencia para variables cualitativas #
# ------------------------------------------------ #

# Lectura de datos
propinas = read.table("propinas.txt",T)

# Estructura de datos
str(propinas)

# Reordenamiento de categor铆as
propinas$dia   = factor(dia,levels(dia)[c(2,4,3,1)])

# Asignaci贸n de variables
attach(propinas)

# Construcci贸n de la tabla usando paquete stats
f     = table(dia)              # Frecuencias absolutas
fr    = prop.table(table(dia))  # Frecuencias relativas
p     = fr*100                  # Frecuencias porcentuales
tabla = cbind(f,fr,p)           # Tabla de frecuencias
tabla

# Construcci髇 de la tabla usando paquete dplyr
library(dplyr)
tabla = propinas %>% 
  count(dia) %>%
  mutate(fr = round(prop.table(n),3)) %>%
  mutate(p  = fr*100) %>%
  arrange(factor(dia,levels(dia)[c(4,1,3,2)]))
tabla
tabla %>% summarise_at(c("n","fr", "p"), sum)

# Actividad: construya las tablas de frecuencia correspondientes para las variables:
# Momento del d韆 (orden de las categor韆s: d韆, noche)
# Nivel de satisfacci髇 (orden de las categor韆s: 5,4,3,2,1)

# ----------------------------------------------------------- #
# Tablas de frecuencia para variables cuantitativas discretas #
# ----------------------------------------------------------- #

# Construcci贸n de la tabla usando paquete stats
f     = table(cantidad)             # Frecuencias absolutas
fr    = prop.table(table(cantidad)) # Frecuencias relativas
p     = fr*100                      # Frecuencias porcentuales
F     = cumsum(f)                   # Frecuencias absolutas acumuladas
Fr    = cumsum(fr)                  # Frecuencias relativas acumuladas
P     = cumsum(p)                   # Frecuencias porcentuales acumuladas
tabla = cbind(f,fr,p,F,Fr,P)        # Tabla de frecuencias
tabla

# Construcci贸n de la tabla usando paquete dplyr
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
