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

# 1. La media o promedio de y
mean(y) 
 
# 2. La cantidad promedio de comensales por mesa
mean(cantidad) 

# 3. El promedio de propina, dividido por sexo
aggregate(propina ~ sexo, propinas, mean) 

# 4. El promedio de propina pagada por hombres
library(dplyr)
propinas %>%
  select(propina) %>%
  filter(sexo == "M") %>% 
  summarise(promedio = mean(propina))

# 5. El promedio de propina, dividido por sexo y momento del día
aggregate(propina ~ sexo + momento, propinas, mean) 

# 6. El promedio de propina pagada por mujeres, durante la noche
propinas %>%
  select(propina) %>%
  filter(sexo == "M" & momento == "Noche") %>% 
  summarise(promedio = mean(propina))

# 7. El promedio de propina, dividido por sexo, momento del día 
# y consumos menores a 30 soles
aggregate(propina ~ sexo + momento + (total < 30), propinas, mean)

# 8. El promedio de propina pagado por mujeres, durante la noche y cuando 
# el consumo es menor a 30 soles
propinas %>%
  select(propina) %>%
  filter(sexo == "M" & momento == "Noche" & total < 30) %>% 
  summarise(promedio = mean(propina))

# Ejercicios
# 9. Obtenga el consumo total medio (19.78594)
# 10. Obtenga el consumo total medio de los fumadores (20.75634)
# 11. Obtenga el número promedio de comensales por mesa durante el día (2.411765)
# 12. Obtenga el monto promedio de propina que dejan los hombres que tienen al menos 3 acompañantes (4.221176)

# ------- #
# Mediana #
# ------- #

# 13. La mediana de y
median(y)

# 14. La mediana de la cantidad de personas por mesa
median(cantidad)

# 15. La mediana del consumo total para mujeres que pagan de día y están acompañadas por más de 3 personas en la mesa
propinas %>%
  select(total) %>%
  filter(sexo == "F" & momento == "Dia" & cantidad > 4) %>% 
  summarise(mediana = median(total))

# Ejercicios
# 16. Determine e interprete el monto mediano de propina 
# 17. Determine e interprete la mediana del nivel de satisfacción
# 18. Determine e interprete la mediana de la propina de los consumos diurnos
# 19. Determine e interprete el monto mediano del consumo total de los no fumadores

# ---- #
# Moda #
# ---- #

# FUNCIÓN MODA
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 20. La moda de y
Mode(y)

# 21. La moda del momento del consumo
Mode(momento)

# 22. La moda del momento del consumo de mujeres que pagan menos de 18 soles por consumo
propinas %>%
  select(momento) %>%
  filter(sexo == "F" & total < 18) %>% 
  summarise(moda = Mode(momento))

# Ejercicios
# 23. Determine e interprete la moda del status de fumador
# 24. Determine e interpetre la moda del nivel de satisfaccción
# 25. Determine e interprete la moda del sexo para los que pagan menos de 5 soles de propina
# 26. Determine e interprete la moda del dia de consumo de las mujeres

# =================== #
# Medidas de Posición #
# =================== #

# ----------- #
# Percentiles #
# ----------- #

# 27. Cuantiles de y
quantile(y)

# 28. Percentil 13 de y
quantile(y, probs = c(0.13))

# 29. Percentiles 10 y 80 (deciles 1 y 8) del consumo total
quantile(total, probs = c(0.1, 0.8))

# 30. Perentiles 30 y 60 del consumo total, dividido por sexo
aggregate(total ~ sexo, propinas, quantile, probs = c(0.3, 0.6))

# 31. Percentiles 30 y 60 del consumo total de hombres
propinas %>% 
  select(total) %>% 
  filter(sexo == "M") %>% 
  summarise(P30 = quantile(total, 0.30),
            P60 = quantile(total, 0.60))

# 32. Percentiles 25, 50 y 90 del consumo total de mujeres  que van de día al restaurante y son acompañadas por más de 3 personas
propinas %>%
  select(total) %>%
  filter(sexo == "F" & momento == "Dia" & cantidad > 4) %>% 
  summarise(Q1  = quantile(total, 0.25),
            Med = quantile(total, 0.50),
            P90 = quantile(total, 0.90))
  
# 33. Determine e interprete el percentil 15 de la propina
# 34. Determine e interprete los percentiles 20 y 65 de la propina dejada por comensales nocturnos
# 35. Determine e interprete el decil 7 del consumo total de fumadores
# 36. Determine e interprete el menor consumo del 20% de hombres que gastaron más
# 37. Determine e interprete el mayor consumo del 32% de fumadores que pagaron menos propina

# ===================== #
# Medidas de Dispersión #
# ===================== #

# ----- #
# Rango #
# ----- #

# 38. Rango de y
diff(range(y))

# 39. Rango del consumo total
diff(range(total))

# 40. Rango del consumo total, por momento de consumo
aggregate(total ~ momento, propinas, range)

# 41. Rango del consumo total diurno
propinas %>% 
  select(total) %>% 
  filter(momento == "Dia") %>% 
  summarise(R = diff(range(total)))

# Ejercicios
# 42. Determine el rango de las propinas durante los días jueves
# 43. Determine el rango del número de comensales por mesa diurna

# --- #
# RIC #
# --- #

# 44. Rango intercuartílico de y
IQR(y)

# 45. Rango intercuartícilo del consumo total
IQR(total)

# 46. Rango intercuartílico del consumo total, dividido por nivel de satisfacción
aggregate(total ~ satisfaccion, propinas, IQR)

# 47. Rango intercuartílico del consumo total para los que están muy insatisfecos
propinas %>% 
  select(total) %>% 
  filter(satisfaccion == "1") %>% 
  summarise(RIC = IQR(total))

# Ejercicios
# 48. Determine e interprete el RIC de las propinas de los días sábados y que son dejadas por mujeres
# 49. Determine e interprete el RIC del consumo total diurno de los jueves

# ------------------- #
# Desviación estándar #
# ------------------- #

# 50. Desviación estándar de y
sd(y)

# 51. Desviación estándar de la cantidad de comensales por mesa
sd(cantidad)

# 52. Desviación estándar de la cantidad de comensales por mesa, dividido por sexo
aggregate(cantidad ~ sexo, propinas, sd)

# 53. Desviación estándar de la cantidad de comensales por mesa cuando paga una mujer
propinas %>% 
  select(cantidad) %>% 
  filter(sexo == "F") %>% 
  summarise(DesvEst = sd(cantidad))

# Ejercicios
# 54. Determine e interprete la desviación estándar del consumo total
# 55. Determine e interprete la desviación estándar de las propinas dejadas por aquellos que están muy satisfechos
# 56. Determine e interprete la desviación estándar del consumo total de hombres, los días viernes por la noche

# -------- #
# Varianza #
# -------- #

# 57. Varianza de y
var(y)

# 58. Varianza de la cantidad de comensales por mesa
var(cantidad)

# 59. Varianza de la cantidad de comensales por mesa, dividido por sexi
aggregate(cantidad ~ sexo, propinas, var)

# 60. Varianza de la cantidad de comensales por mesa, cuando paga una mujer
propinas %>% 
  select(cantidad) %>% 
  filter(sexo == "F") %>% 
  summarise(Var = var(cantidad))


# --------------------------- #
# Coeficiente de variabilidad #
# --------------------------- #

sd(y)/abs(mean(y))

sd(total)/abs(mean(total))
