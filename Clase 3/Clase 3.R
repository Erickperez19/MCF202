#Erick Raymundo Pérez Silva
#Clase 3
# 07/08/19


# Ejercicio 1 de comparacion de medias --------------------------------------


Grupo <- gl(2, 12, labels = c("Fotografía", "Araña"))
Ansiedad <- c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45, 40, 50, 40, 35, 50, 55,
              65, 55, 50, 35, 30, 50, 60, 39)
Datos <-data.frame(Grupo, Ansiedad)
head(Datos)
summary(Datos)
boxplot(Datos$Ansiedad ~ Datos$Grupo, col= "blue", ylab = "Nivel de ansiedad")
length(Datos$Grupo)
tapply(Datos$Ansiedad, Datos$Grupo, mean)

shapiro.test(Datos$Ansiedad)

bartlett.test(Datos$Ansiedad, Datos$Grupo)
library(pastecs)
by(Datos$Ansiedad, Datos$Grupo, stat.desc, basic= FALSE, norm= TRUE)

gr.t <- t.test(Datos$Ansiedad ~ Datos$Grupo, var.equal = TRUE)
gr.t


# Ejercicio 2 de comparacion de medias ------------------------------------

## La hipotesis nula sera que no existen diferencias entre la media es igaul a 80 kg, 
##y la hipotesis alternativa es que la media observada es menor a 80kg

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)
mean(costal)

##Determinar el número de observaciones
n  <- length(costal)
n

##Determinar la media
costa.media  <- mean(costal)
costa.media

##Desviación estandar

costa.sd  <- sd(costal)
costa.sd

cost.se  <- costa.sd/ sqrt(n)
cost.se

cost.T <- ((costa.media-80)/ cost.se)
cost.T

pt(cost.T, df = n-1)

t.test(costal, mu= 80, alternative = "less")
