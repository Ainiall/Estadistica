##ejemplo 3.7
n <- 1000
vida1 <- rexp(n,rate=0.1)
vida2 <- rexp(n,rate=0.15)
vida3 <- rweibull(n,shape=1,scale=0.13)

##serie vida1 vida2, paralelo resultado con vida3
vidaS <- pmax(vida3,pmin(vida1,vida2))
vidaM <- mean(vidaS)
prob <- mean(vidaS >=5)
##tambien se puede poner como
datos.vidas <- data.frame(vidaS) ##se pone como datos activos


library(abind, pos=16)
library(e1071, pos=17)

