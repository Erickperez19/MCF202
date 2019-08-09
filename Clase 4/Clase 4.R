#Erick Raymundo Pérez Silva
#Clase 4
#09/08/19


# Correlacion -------------------------------------------------------------

library(repmis)
erupciones <- source_data("https://www.dropbox.com/s/liir6sil7hkqlxs/erupciones.csv?dl=1")

plot(log(erupciones$waiting), log(erupciones$eruptions), pch=6, col= "blue", 
     xlab = "Tiempo de espera (min)",
     ylab= "Duracion (min)")

##es una relacion positiva ya que mayor sea el tiempo de espera la duracion aumenta.

library(pastecs)
stat.desc(erupciones$eruptions,  basic= FALSE, norm= TRUE)

shapiro.test(erupciones$eruptions)
shapiro.test(erupciones$waiting)
# cuando la variable involucra tiempo, casi nunca tienen una distribucion normal              
             
cor.test(erupciones$eruptions, erupciones$waiting)
#ho no es significativa (mayor 0.05)
#ha es significativa (menor 0.05)
#la correlacion es significativa  


# Ejercicio 1 -------------------------------------------------------------

DB_ebanos  <- read.csv("C:/MCF202_2019/Datos/ebanos.csv", header = T)

plot(DB_ebanos$altura, DB_ebanos$diametro, col= "blue", pch= 1,
     xlab = "Diametro (cm)",
     ylab = "Altura (m)")
stat.desc(DB_ebanos$altura,  basic= FALSE, norm= TRUE)
shapiro.test(DB_ebanos$altura)

cor.test(DB_ebanos$altura, DB_ebanos$diametro)
##La correalcion es significativa 

# Regresion lineal --------------------------------------------------------

##ho no es significativa la prediccion
##ha es significativa la prediccion

#comando "lm" para realizar la regresion
lm.erup <-lm(erupciones$eruptions ~ erupciones$waiting)
## primero es la independiente 

plot(erupciones$waiting, erupciones$eruptions, pch=1, col= "blue", 
     xlab = "Tiempo de espera (min)",
     ylab= "Duracion (min)")
abline(lm.erup, col= "black")
## en la grafica primero es x y despues y
##abline es para que salga la linea.

text(52, 4.5, "Y = -1.87 + 0.07*x", pos= 1)
text(52, 4, "r^2= 0.81")
lm.erup
## informacion de alfa y beta.

##los datos deben de estar bien distibuidos por abajo y arriba de la linea.

summary(lm.erup)
## los reiduales es la diferencia que existe  entre el valor observado 
##y el valor predecido.
## cuando el valor observado es mayor , es positivo 

length(erupciones$eruptions)

y.60 <- -1.87 + 0.07*60
y.60


y.80 <- -1.87 + 0.07*80
y.80


# Datos de regresion ------------------------------------------------------

espera <- erupciones$waiting
duracion <- erupciones$eruptions

res <- resid(lm.erup)
res
pre <- fitted(lm.erup)

res.2 <- res^2 

cuadro <- data.frame(espera, duracion, pre, res, res.2)

cuadro <- round(data.frame(espera, duracion, pre, res, res.2),4)

SSE <- sum((duracion - pre)^2)
SSE


vari <- SSE/ (length(erupciones$waiting)-2)
vari


# Prueba de hipotesis de la regresion -------------------------------------

an.erup <- anova(lm.erup)
an.erup

##media de cuadrado= suma de cuadrado/ grados de libertad 


## se acepta la hipotesis alternativa quiere decir que el modelo de regresion
##son significativos 