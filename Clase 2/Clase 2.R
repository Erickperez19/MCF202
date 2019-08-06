# Erick Perez Silva
# 06/08/2019
# Clase 2


# Importar datos ----------------------------------------------------------

vivero <- read.csv("C:/MCF202_2019/Datos/Tvivero.csv", header = T)
summary(vivero)


# Prueba de t de una muesta -----------------------------------------------
par(mfrow=c(1,1))
boxplot(vivero$IE)
t.test(vivero$IE, mu = 0.85)
#La medaia observada no es diferente estadisticamente ya que el valor de p
#es mayor que el alfa establecido (0.05). Ademas la media teoretica se encuentra dentro
#del rango de los valores de intervalos de confianza.

t.test(vivero$IE, mu = 0.90)
#La media observada es difernte a la media teoretica, por la cual aceptamos la H1.
#valor de p (0.01) es menor que el valor de alfa establecido (0.05).


# Prueba de t muestras independientes  ------------------------------------

boxplot(vivero$IE ~ vivero$Tratamiento, col= "blue", xlab= "tratamiento", ylab= "IE")

shapiro.test(vivero$IE)

var.test(vivero$IE ~ vivero$Tratamiento)
#Las varianzas de ambos tratamientos son iguales asi lo pureba el valor de p
#obtenido mediante una prueba de varianza (var.test).

t.test(vivero$IE ~ vivero$Tratamiento, var.equal = T)

#Existe una diferancia significativa  ante el IE de las plantulas fertilizadas
#El valor de p (0.004) comprueba nuestra hipotesis de que el fertilizante "power" 
#mejora el IE 