##Erick Raymundo Pérez Silva
## Clase 6
##09/08/19


# Analisis de covarianza ----------------------------------------------------------------


edad <- read.csv("C:/MCF202_2019/Datos/datos_control_Rascon.csv", header = T)
head(edad)
str(edad)

##Identificar columna SP como factor

edad$SP  <-  factor(edad$SP)


# Separar factor ----------------------------------------------------------

ariz  <-  subset(edad, SP == "arizonica")
ariz.lm <- lm(ariz$EDAD ~ ariz$DAP)
summary(ariz.lm)


dura  <-  subset(edad, SP == "durangensis")


# Regresion dos facotres-------------------------------------------------------------------------

cov.edad  <- lm(edad$EDAD ~ edad$DAP + edad$SP)
summary(cov.edad)

plot(edad$DAP[edad$SP == "arizonica"], edad$EDAD[edad$SP == "arizonica"],
     col="red", pch= "A",
     xlim = c(0,50),
     ylim = c(0,130))

abline(cov.edad$coefficients[1], cov.edad$coefficients[2], col="red")
text(30, 20, "Ya = -7.65 + 1.98*x", pos = 2)
points(edad$DAP[edad$SP == "durangensis"], edad$EDAD[edad$SP == "durangensis"],
       col="blue", pch= "D")
abline(cov.edad$coefficients[1] + cov.edad$coefficients[3],
       cov.edad$coefficients[2], col="blue", lty= "dashed")
text(19, 100, "Yd= 11.41 + 1.98*x")

## se acepta la hipotesis alternativa ya que si hay diferencias sigificativas
