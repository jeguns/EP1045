# ==================================== #
# CAP?TULO II: ESTAD?STICA DESCRIPTIVA #
# ==================================== #

# Vamos a utilizar el archivo de datos propinas.txt
# Archivo de datos y c?digos disponibles en https://github.com/jeguns/EP1045

# ------------------------------------ #
# Gráficos para variables cualitativas #
# ------------------------------------ #

# Lectura de datos
propinas = read.table("propinas.txt",T)
attach(propinas)

# Gráfico de barras verticales usando stats

barplot(table(dia))

barplot(table(dia), 
        col  = "red", 
        xlab = "D?a",
        ylab = "N?mero de consumos",
        main = "Consumos por D?a")

# Gráfico de barras verticales usando ggplot2

library(ggplot2)

graf.barrasV = ggplot(data = propinas, aes(x = dia)) + 
  geom_bar() 
graf.barrasV

graf.barrasV = ggplot(data = propinas) + 
  aes(x = dia) +
  geom_bar() 
graf.barrasV 

graf.barrasV = ggplot(data = propinas, aes(x = dia, fill = "red")) + 
  geom_bar() +
  geom_text(stat='count', aes(label = ..count..), vjust = 2) +
  labs(x = "D?a", y = "N?mero de consumos", title = "Consumos por D?a") +
  theme(legend.position = "none")
graf.barrasV

graf.barrasV = ggplot(data = propinas, aes(x = factor(dia,levels(dia)[c(2,4,3,1)]), fill = "red")) + 
  geom_bar() +  
  geom_text(stat='count', aes(label = ..count..), vjust = 2) +
  labs(x = "D?a", y = "N?mero de consumos", title = "Consumos por D?a") +
  theme(legend.position = "none") 
graf.barrasV

library(forcats)
graf.barrasV = ggplot(data = propinas, aes(x = fct_infreq(dia), fill = dia)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label = ..count..), vjust = 2) + 
  labs(x = "D?a", y = "N?mero de consumos", title = "Consumos por D?a") +
  theme(legend.position = "none")
graf.barrasV

# Gráfico de barras horizontales

graf.barrasH = ggplot(data = propinas, aes(x = fct_infreq(dia), fill = dia)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label = ..count..), hjust = 1.5) +
  labs(x = "Día", y = "Número de consumos", title = "Consumos por Día") +
  theme(legend.position = "none") +
  coord_flip()
graf.barrasH

# Gráfico circular

pie = ggplot(propinas, aes(x = "", fill = factor(dia))) + 
  geom_bar(width = 1) +
  coord_polar(theta = "y") 
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

