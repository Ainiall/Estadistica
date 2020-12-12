#si no se indica, R asume que los procesos son equiprobables
x <- c(0,1)
n <- 10000
 
muestra <- sample(x,n,replace=TRUE, prob=c(0.3,0.7)) #cambia la probabilidad
FA <-cumsum(muestra)
Media_acumulada <- FA/1:n #vector de 1 a n

plot(Media_acumulada,type="l") #tipo L (linea)
abline(0.5,0,col="red") #paralela al eje x

dado <- c(1,2,3,4,5,6)
n_dados <- 4
muestra2 <-sample(dado,n_dados,replace=TRUE)
sum(muestra2)

suma_dados <- replicate(10000,sum(sample(dado,n_dados,replace=TRUE)))
tabla_dados <- data.frame(suma_dados)
tabla_dados$suma_factor <- as.factor(tabla_dados,dados$suma_dados)
frec <- table(suma_factor)
#para crear una nueva columna se usa $ ej: tabla_dados$nuevacol
porcentaje <- 100*frec/sum(frec)

barplot(frec,xlab="nombre del eje x", ylab="nombre del eje y",col="blue")
#similar a una campana de gauss



