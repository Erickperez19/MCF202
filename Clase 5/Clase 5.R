#Erick Raymundo Pérez Silva
#Clase 5
#09/08/19

# ANOVA -------------------------------------------------------------------

#ho no existe diferencia entre los tratamientos
#ha existe una diferencia entre los tratamientos 

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

