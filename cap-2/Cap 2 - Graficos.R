# ==================================== #
# CAPÍTULO II: ESTADÍSTICA DESCRIPTIVA #
# ==================================== #

# Vamos a utilizar el archivo de datos propinas.txt
# Archivo de datos y códigos disponibles en https://github.com/jeguns/EP1045

# ------------------------------------ #
# Gráficos para variables cualitativas #
# ------------------------------------ #

# Lectura de datos
propinas       = read.table("propinas.txt",T)
propinas$dia   = factor(propinas$dia,levels(propinas$dia)[c(2,4,3,1)])
attach(propinas)

# Gráfico de barras verticales usando stats

barplot(table(dia))

barplot(table(dia), 
        col  = "red", 
        xlab = "Día",
        ylab = "Número de consumos",
        main = "Consumos por Día")

# Gráfico de barras verticales usando ggplot2

library(ggplot2)

ggplot(data = propinas, aes(x = dia)) + geom_bar()

ggplot(data = propinas, aes(x = dia)) + 
  geom_bar() 

graf.barrasV = ggplot(data = propinas, aes(x = dia)) + 
  geom_bar() 
graf.barrasV

graf.barrasV = ggplot(data = propinas, aes(x = dia, fill = "red")) + 
  geom_bar() +
  geom_text(stat='count', aes(label = ..count..), vjust = 2) +
  labs(x = "Día", 
       y = "Número de consumos", 
       title = "Consumos por Día",
       caption = "Fuente: Restaurante") +
  theme(legend.position = "none")
graf.barrasV

library(forcats)
graf.barrasV = ggplot(data = propinas, aes(x = fct_infreq(dia), fill = dia)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label = ..count..), vjust = 2) + 
  labs(x = "Día", 
       y = "Número de consumos", 
       title = "Consumos por Día",
       caption = "Fuente: Restaurante") +
  theme(legend.position = "none")
graf.barrasV

# Gráfico de barras horizontales

graf.barrasH = ggplot(data = propinas, aes(x = fct_infreq(dia), fill = dia)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label = ..count..), hjust = 1.5) +
  labs(x = "Día", 
       y = "Número de consumos", 
       title = "Consumos por Día",
       caption = "Fuente: Restaurante") +
  theme(legend.position = "none") +
  coord_flip()
graf.barrasH

# Gráfico circular

pie = ggplot(propinas, aes(x = "", fill = factor(dia))) + 
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
pie

pie = ggplot(propinas, aes(x = "", fill = factor(dia))) + 
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme(plot.title  = element_text(hjust=0.5),
        panel.grid  = element_blank(),
        axis.text.x = element_blank()) + 
  labs(fill  = "dia", 
       x     = NULL, 
       y     = NULL, 
       title = "Distribución de consumos por día", 
       caption = "Fuente: Restaurante") 
pie

library(dplyr)
tabla = propinas %>% 
  count(dia) %>%
  mutate(fr = round(prop.table(n),3)) %>%
  mutate(p  = fr*100) 

pie = ggplot(tabla, aes(x = "", y = n, fill = dia))+
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(x = 1.25, 
                y = n, 
                label = n), position = position_stack(vjust = 0.5)) + 
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x=element_blank()) +
  coord_polar(theta = "y") +
  labs(fill  = "dia", 
       x     = NULL, 
       y     = NULL, 
       title = "Distribución de consumos por día", 
       caption = "Fuente: Restaurante")   
pie

# ----------------------------------------------- #
# Gráficos para variables cuantitativas discretas #
# ----------------------------------------------- #

# Gráfico de varas

tabla = propinas %>% 
  count(cantidad) %>%
  mutate(fr = prop.table(n)) %>%
  mutate(p  = fr*100) %>%
  mutate(F  = cumsum(n)) %>%
  mutate(Fr = cumsum(fr)) %>%
  mutate(P  = cumsum(p))
tabla

varas = ggplot(tabla, aes(x = cantidad, y = n)) + 
  geom_point(size=3) + 
  geom_segment(aes(x = cantidad, 
                   xend = cantidad, 
                   y=0, 
                   yend = n)) + 
  labs(x       = "Número de comensales",
       y       = "Frecuencia absoluta",
       title   = "Distribución de la cantidad de comensales", 
       caption = "Fuente: Restaurante")  +
  geom_text(aes(label = n), vjust = -1) +
  ylim(0,175)

varas

# ----------------------------------------------- #
# Gráficos para variables cuantitativas continuas #
# ----------------------------------------------- #

# Histograma

ggplot(data=propinas, aes(total)) + 
  geom_histogram()

qplot(total, geom="histogram") 

ggplot(data=propinas, aes(total)) + 
  geom_histogram(bins = 10)

qplot(total, geom="histogram", bins=10) 

ggplot(data=propinas, aes(total)) + 
  geom_histogram(binwidth = 10)

qplot(total, geom="histogram", binwidth = 10) 

ggplot(data=propinas, aes(total)) + 
  geom_histogram(bins = 8,col = I("red"))

qplot(total, geom="histogram", bins = 8, col = I("red")) 

ggplot(data=propinas, aes(total)) + 
  geom_histogram(bins = 8,col = I("red"),fill = I("blue"))

qplot(total, geom="histogram", bins = 8, col = I("red"), fill = I("blue"))

ggplot(data=propinas, aes(total)) + 
  geom_histogram(bins = 8,col = I("red"),fill = I("blue")) +
  labs(x = "Consumo total",
       y = "Frecuencia",
       title = "Distribución de montos de consumo",
       caption = "Fuente: Restaurante")

qplot(total, geom="histogram", bins = 8, col = I("red"), fill = I("blue"), 
      xlab = "Consumo total", ylab = "Frecuencia", main = "Distribución de montos de consumo")

# Colores en R --> http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Polígono de frecuencias

ggplot(data=propinas, aes(total)) + 
  geom_freqpoly()

ggplot(data=propinas, aes(total)) + 
  geom_freqpoly(bins = 10)

ggplot(data=propinas, aes(total)) + 
  geom_freqpoly(bins = 10, col = I("red"))

ggplot(data=propinas, aes(total)) + 
  geom_freqpoly(bins = 10, col = I("red"), lwd = 1.25)

ggplot(data=propinas, aes(total)) + 
  geom_freqpoly(bins = 10, col = I("midnightblue"), lwd = 1.25) +
  labs(x = "Consumo total",
       y = "Frecuencia",
       title = "Distribución de montos de consumo",
       caption = "Fuente: Restaurante")

# Diagrama de cajas o Boxplot

ggplot(data=propinas, aes(y = total)) + 
  geom_boxplot()  
  
ggplot(data=propinas, aes(y = total)) + 
  geom_boxplot(col = I("blue"), fill = I("gold"))  

ggplot(data=propinas, aes(y = total)) + 
  geom_boxplot(col = I("blue"), fill = I("gold")) +
  labs(x = '',
       y = "Consumo total",
       title = "Distribución de montos de consumo",
       caption = "Fuente: Restaurante")

ggplot(data=propinas, aes(x = dia, y = total)) + 
  geom_boxplot(col = I("blue"), fill = I("gold")) +
  labs(x = 'Día',
       y = "Consumo total",
       title = "Distribución de montos de consumo",
       caption = "Fuente: Restaurante")

# Ojiva

ggplot(data=propinas, aes(x=total)) + 
  geom_step(stat = "ecdf", col = I("blue"), lwd=1.3) + 
  labs(x = "Consumo total",
       y = "Frecuencia relativa acumulada",
       title = "Distribución de montos de consumo",
       caption = "Fuente: Restaurante")
  
