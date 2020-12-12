## test de normalidad
normalityTest(~consumo, test="shapiro.test", data=acero2)
## medida de muestras
with(acero2, (t.test(consumo, alternative='two.sided', mu=120.0, conf.level=.95)))
## se rechaza el resultado segun los datos obtenidos (p-valor 0.000221 < 0.05)
## hay evidencias para SUPONER que el consumo es diferente a 120

##se trabaja con la hipotesis alternativa porque es la que se acepta o no 
## (h0 supone, no afirma)
with(acero2, (t.test(consumo, alternative='less', mu=140.0, conf.level=.95)))
## no hay evidencias suficientes en la muestra para aifmrar eso al 95%

## el primer paso es siempre hacer un test de normalidad
normalityTest(~pr.galv1, test="shapiro.test", data=acero2)
## como los datos no son normales, se debe usar medidas de centralizacion (h0: Me, h1: Me)

## en ese caso se hacen test no parametricos 
with(acero2, median(pr.galv1, na.rm=TRUE))
with(acero2, mean(pr.galv1, na.rm=TRUE))
with(acero2, wilcox.test(pr.galv1, alternative='less', mu=400.0))
## no se rechaza la hipotesis nula de que la mediana sea mayor o igual de 400 al 95%
## por lo tanto no hay suficientes evidencias para afirmar que sea cierto al 95%

## p es el primer valor por orden alfabetico
## proporciones -> bonimial exacto
local({
  .Table <- xtabs(~ averias , data= acero2 )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  binom.test(rbind(.Table), alternative='less', p=.90, conf.level=.95)
})
## hay suficientes evidencias para probar que se rechaza

## es mas rapido reordenar las variables para que use p con el valor que queremos
acero2$averias <- with(acero2, factor(averias, levels=c('Si','No')))
local({
  .Table <- xtabs(~ averias , data= acero2 )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  binom.test(rbind(.Table), alternative='greater', p=.1, conf.level=.95)
})

