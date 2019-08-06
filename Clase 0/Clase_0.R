# Erick Perez Silva
# 05/08/2019
# Clase 0

 
# Pasos basicos -----------------------------------------------------------

2+2
a <- 2
a*a
a+5


# Importar datos ----------------------------------------------------------

diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)
diametro

#Medidas de tendencia central
mean(diametro)
median(diametro)

#Medias de dispersion

var(diametro)
sd(diametro)


# Graficas ----------------------------------------------------------------

boxplot(diametro, horizontal = TRUE, col = "blue", main="diametro",
        xlab="D (cm)")
# Importar excel ----------------------------------------------------------

DB_alturas <- read.csv("C:/MCF202_2019/Datos/alturas.csv", header = T)
head(DB_alturas)
boxplot(DB_alturas$crecimiento)
boxplot(DB_alturas$crecimiento ~ DB_alturas$tratamiento, col ="blue",
        xlab= "tratamiento", ylab= "crecimiento")
mean(DB_alturas$crecimiento)

# Restrincciones ----------------------------------------------------------

sum(DB_alturas$crecimiento < mean(DB_alturas$crecimiento))

#Excluir el Tratamiento A
Trat_A <- DB_alturas[!(DB_alturas$tratamiento == "TA"),]

mean(Trat_A$crecimiento)


# Submuestra --------------------------------------------------------------

T.mean <- subset(DB_alturas, crecimiento >= mean(DB_alturas$crecimiento))
boxplot(T.mean$crecimiento ~ T.mean$tratamiento)

