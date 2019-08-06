# Erick Perez Silva
# 06/08/2019
# Clase 1


# Importar datos ----------------------------------------------------------

dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1, 
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3, 9.7, 6.5, 
         23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)

dbh
length(dbh)

##media

sum(dbh)/length(dbh)

mean(dbh)
median(dbh)

fivenum(dbh)
boxplot(dbh)
range(dbh)

##Grafica 

stem(dbh)
hist(dbh)


moda=function(x)  
  
  #Función que encuentra la moda de un vector x
  m1 <- sort(table(x),decreasing=T)
moda <- names(m1[m1==m1[1]])
moda <- as.numeric(moda)
return(moda)

quantile (dbh, 0.25)
quantile(dbh, 0.5)      
quantile(dbh, 0.75 )

fivenum(dbh)

par(mfrow=c(2,2))
set.seed(10)
dbh.10 <- rnorm(10)
hist(dbh.10)
dbh.50 <- rnorm(50)
hist(dbh.50)
dbh.500 <- rnorm(500)
hist(dbh.500)
dbh.1000 <- rnorm(1000)
hist(dbh.1000)


#porbar normalidad de datos

shapiro.test(dbh)

 
