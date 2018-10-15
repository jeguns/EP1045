# ==================================== #
# CAPÍTULO II: ESTADÍSTICA DESCRIPTIVA #
# ==================================== #

# Vamos a utilizar el archivo de datos propinas.txt
# Archivo de datos y códigos disponibles en https://github.com/jeguns/EP1045

propinas       = read.table("propinas.txt",T)
attach(propinas)

y = c(3,4,0,-1,3)

# ---------------------------- #
# Medidas de Tendencia Central #
# -----------------------------#

# ---------------- #
# Media Aritmérica #
# ---------------- #

mean(y) # media de y
 
mean(cantidad) # cantidad promedio de comensales por mesa

# promedio de propina, dividido por sexo
aggregate(propina ~ sexo, propinas, mean) 

library(dplyr)
propinas %>%
  select(propina) %>%
  filter(sexo == "M") %>% 
  summarise(promedio = mean(propina))

# promedio de propina, dividido por sexo y momento del día
aggregate(propina ~ sexo + momento, propinas, mean) 

propinas %>%
  select(propina) %>%
  filter(sexo == "M" & momento == "Noche") %>% 
  summarise(promedio = mean(propina))

# promedio de propina, dividido por sexo, momento del día y consumos menores a 30 soles
aggregate(propina ~ sexo + momento + (total < 30), propinas, mean)

propinas %>%
  select(propina) %>%
  filter(sexo == "M" & momento == "Noche" & total < 30) %>% 
  summarise(promedio = mean(propina))

# Ejercicios
# 1. Obtenga el consumo total medio (19.78594)
# 2. Obtenga el consumo total medio de los fumadores (20.75634)
# 3. Obtenga el número promedio de comensales por mesa durante el día (2.411765)
# 4. Obtenga el monto promedio de propina que dejan los hombres que tienen al menos 3 acompañantes (4.221176)


# ------- #
# Mediana #
# ------- #

median(y)

propinas %>%
  select(total) %>%
  filter(sexo == "F" & momento == "Dia" & cantidad > 4) %>% 
  summarise(mediana = median(total))

# ---- #
# Moda #
# ---- #

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(y)

Mode(momento)

propinas %>%
  select(momento) %>%
  filter(sexo == "F" & total < 18) %>% 
  summarise(moda = Mode(momento))

# =================== #
# Medidas de Posición #
# =================== #

# ----------- #
# Percentiles #
# ----------- #

quantile(y)

quantile(y, probs = c(0.13))

quantile(total, probs = c(0.1, 0.8))

aggregate(total ~ sexo, propinas, quantile, probs = c(0.3, 0.6))

propinas %>% 
  select(total) %>% 
  filter(sexo == "M") %>% 
  summarise(P30 = quantile(total, 0.30),
            P60 = quantile(total, 0.60))

propinas %>%
  select(total) %>%
  filter(sexo == "F" & momento == "Dia" & cantidad > 4) %>% 
  summarise(Q1  = quantile(total, 0.25),
            Med = quantile(total, 0.50),
            P90 = quantile(total, 0.90))
  
# ===================== #
# Medidas de Dispersión #
# ===================== #

# ----- #
# Rango #
# ----- #

diff(range(y))

diff(range(total))

aggregate(total ~ momento, propinas, range)

propinas %>% 
  select(total) %>% 
  filter(momento == "Dia") %>% 
  summarise(R = diff(range(total)))

# --- #
# RIC #
# --- #

IQR(y)

IQR(total)

aggregate(total ~ satisfaccion, propinas, IQR)

propinas %>% 
  select(total) %>% 
  filter(satisfaccion == "3") %>% 
  summarise(RIC = IQR(total))

# ------------------- #
# Desviación estándar #
# ------------------- #

sd(y)

sd(cantidad)

aggregate(cantidad ~ sexo, propinas, sd)

propinas %>% 
  select(cantidad) %>% 
  filter(sexo == "F") %>% 
  summarise(DesvEst = sd(cantidad))

# -------- #
# Varianza #
# -------- #

var(y)

var(cantidad)

aggregate(cantidad ~ sexo, propinas, var)

propinas %>% 
  select(cantidad) %>% 
  filter(sexo == "F") %>% 
  summarise(Var = var(cantidad))


# --------------------------- #
# Coeficiente de variabilidad #
# --------------------------- #

sd(y)/abs(mean(y))

sd(total)/abs(mean(total))
