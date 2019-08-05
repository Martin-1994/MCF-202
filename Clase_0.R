# Martin Lopez
# 05/08/2019
# clase 0

# pasos básicos -----------------------------------------------------------

2+2 
a <- 1

diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)
diametro

# Medidas de tendencia central

mean(diametro)
median(diametro)

# Medidas de dispersión

sd(diametro)
var(diametro)

# Gráficas ----------------------------------------------------------------

boxplot(diametro, horizontal =TRUE, col = "lightblue", main= "diametro", 
        xlab= "D (cm)")


