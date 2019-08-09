# Martin Lopez Ortiz
# 09/08/2019
# Clase 4

# Ejercicio 1
#  Subir datos ------------------------------------------------------------

erupciones <-read.csv("C:/MCF202_2019/Datos/erupciones.csv", header = T)
summary(erupciones)

plot(log(erupciones$waiting), log(erupciones$eruptions), pch=17, col= "blue", xlab= "Tiempo de espera (min)",
     ylab= "Duracion (min)")
library(pastecs)
stat.desc(erupciones$eruptions, basic = FALSE, norm = TRUE)

shapiro.test(erupciones$eruptions)
shapiro.test(erupciones$waiting)

cor.test(erupciones$eruptions, erupciones$waiting)

# Ejercicio 2

# Subir Datos -------------------------------------------------------------

ebanos <-read.csv("C:/MCF202_2019/Datos/ebanos.csv", header = T)
summary(ebanos)

plot(log(ebanos$diametro), log(ebanos$altura), pch=14, col= "red", xlab= "diametro (cm)",
     ylab= "altura (m)")
library(pastecs)
stat.desc(ebanos$altura, basic = FALSE, norm = TRUE)

shapiro.test(ebanos$diametro)
shapiro.test(ebanos$altura)

cor.test(ebanos$diametro, ebanos$altura)

# De cuerdo a los datos obtenidos en la prueba de normalidad 
# de los datos, la correlacion es significativa ya que p-value
# es menor a 0.05 de los niveles de confianza, en conclusion
# las variables tienen alta significancia por lo que se rechaza H0
# y se acepta Ha.
