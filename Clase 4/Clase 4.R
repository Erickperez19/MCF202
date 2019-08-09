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




