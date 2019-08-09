# Martin Lopez Ortiz
# 09/08/2019
# Clase 4

# Ejercicio 1
#  Subir datos ------------------------------------------------------------

erupciones <-read.csv("C:/MCF202_2019/Datos/erupciones.csv", header = T)
summary(erupciones)

plot(erupciones$waiting, erupciones$eruptions, pch=17, col= "blue", 
     xlab= "Tiempo de espera (min)",
     ylab= "Duracion (min)")
library(pastecs)
stat.desc(erupciones$eruptions, basic = FALSE, norm = TRUE)

shapiro.test(erupciones$eruptions)
shapiro.test(erupciones$waiting)

cor.test(erupciones$eruptions, erupciones$waiting)

# Regresion lineal --------------------------------------------------------

lm.erup <- lm(erupciones$eruptions ~ erupciones$waiting)
plot(erupciones$waiting, erupciones$eruptions, pch=17, col= "blue", 
     xlab= "Tiempo de espera (min)",
     ylab= "Duracion (min)")
abline(lm.erup, col= "red")
text(52, 4.5, "Y = -1.87 + 0.07*x")
text(52, 3, "r^2 = 0.81")
lm.erup
summary(lm.erup)

length(erupciones$eruptions)
y.60 <- -1.87 + 0.7*60
y.60


# Datos de regresion ------------------------------------------------------

espera <- erupciones$waiting
duracion<-erupciones$eruptions
res <- resid(lm.erup)
pre <- fitted(lm.erup)
res.2 <-res^2

cuadro <- round(data.frame(espera,duracion,pre,res,res.2),4)

SSE <- sum(cuadro$res.2)
SSE

vari <- SSE/(length(erupciones$waiting)-2)
vari


# Prueba de hipotesis de la regresion -------------------------------------

an.erup <- anova(lm.erup)
an.erup

# Se acepta Ha la regresion se puede aplicar debido a < 2.2e-16
