#Erick Raymundo Pérez Silva
#Clase 5
#09/08/19

# ANOVA -------------------------------------------------------------------

#ho no existe diferencia entre los tratamientos
#ha al menos un grupo es difernete a los demas 

arena <- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo <- c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)

y.ton <- c(arena, arcilla, limo)
suelo <- gl(3, 10, 30, labels =c("arena", "arcilla", "limo"))

prod <- data.frame(suelo, y.ton)
head(prod)

tapply(prod$y.ton, prod$suelo, mean)
tapply(prod$y.ton, prod$suelo, var)

shapiro.test(prod$y.ton)

## Sirve para la homogeniedad de varianzas
bartlett.test(prod$y.ton, prod$suelo)

fligner.test(prod$y.ton, prod$suelo)
## las varianzas son homogeneas


boxplot(prod$y.ton ~ prod$suelo, xlab = "Tipo de suelo",
        ylab = "Ton/ha", col= "blue")
##ANOVA
aov.suelo <- aov(prod$y.ton ~ prod$suelo)
aov.suelo
summary(aov.suelo)

par(mfrow=c(2,2))
plot(aov(prod$y.ton ~ prod$suelo))
par(mfrow=c(1,1))
## si los datos estan cerca de la lina central en la grafica "nomal Q-Q"
## quiere decir que los datos vienen de una distibucion normal


##prueba de tukey sirve para saber cual de los tratamientos es diferente,
## 3 o mas medias

TukeyHSD(aov.suelo, conf.level = 0.95)

##lwr rango menor
##upr rango mayor

plot(TukeyHSD(aov.suelo))
summary.lm(aov.suelo)

## se acpeta la hipotesis alternativa ya que, al menos uno de los grupos
## es diferente a los demas 