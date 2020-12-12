## X~B(n,px) e Y~B(m,py) se va a contrastar si son iguales o no.
## H0 px =py -> px-py = 0   	H0 px-py >/0   H0 px-py \<0
## H1 px!=py -> px-py != 0 	 H1 px-py \<0 		H1 px-py >/0
## Esto pasa a ser:
## Xoff y Xon ~B(n,pxoff) y B(m,pxon), donde x es porcentaje de no averia

load("F:/ESTADISTICA/acero2.rda")

##¿porcentaje de horas en las que averias es menor cuando esta apagado el sistema o cuando esta encendido?

##proporciones para 2 muestras, hipotesis alternativa < para poder afirmar o no la pregunta
local({  .Table <- xtabs(~sistema+averias, data=acero2)
  cat("\nPercentage table:\n")
  print(rowPercents(.Table))
  prop.test(.Table, alternative='less', conf.level=.95, correct=FALSE)
})
##CONCLUSION: Como pvalor> alpha no se rechaza la hipotesis nula al 95%. 
## No se puede afirmar porque no hay evidencias suficientes que lo indiquen al 95%.

##Mirando exclusivamente el intervalo de confianza se puede saber si se rechaza o no:
##porque hay valores positivos y la hipotesis nula es h0: poff-pon>0, por lo que
##podria ser cierto en un 95% de los casos


##Si son independientes se debe hacer un test de normalidad
##se hace un test por grupos
normalityTest(consumo ~ sistema, test="shapiro.test", data=acero2)
acero2$diferencia <- with(acero2, pr.galv1- pr.galv2)

