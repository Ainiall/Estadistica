### Conjunto de datos acero2
summary(acero2)

### 1- Apoyan estos datos la hipotesis de que el nivel medio de emision de dioxido de carbono (C02) 
### supera las 100 t/h?
	# El primer paso es realizar un test de normalidad
	normalityTest(~CO2, test="shapiro.test", data=acero2)
	#Como p-valor>>>alpha, no se rechaza la hipotesis nula Es decir, no hay evidencias en contra de 
	#suponer la normalidad de la variable.
	#Se usa el test t para una muestra H0 =< 100 y alternativa H1>100 
	#(PREGUNTA SIEMPRE EN LA ALTERNATIVA)
	with(acero2, (t.test(CO2, alternative='greater', mu=100.0, conf.level=.95)))
	## Como p-valor>>> alpha, no se rechaza la hipotesis nula. Por lo tanto no se 
	## puede afirmar, sino suponer.

	## No hay evidencias al 95% en contra de suponer que el nivel medio de emision supera las 100t/h

### 2- Apoyan estos datos la hipotesis de que el consumo promedio es menos de 130 megativos-hora?
	# El primer paso es realizar un test de normalidad
	normalityTest(~consumo, test="shapiro.test", data=acero2) 
	#Como p-valor>>>alpha, no se rechaza la hipotesis nula. Es decir, no hay evidencias en contra de 
	#suponer la normalidad de la variable.
	#Se usa el test t para una muestra  H0 : µ ≥ 130 frente a la alternativa H1 : µ < 130 
	with(acero2, (t.test(consumo, alternative='less', mu=130.0, conf.level=.95))
	## El p-valor es 0.9668 >>> alpha, por lo que no se rechaza la hipotesis nula.

	# No hay evidencias al 95% en contra de suponer que el consumo medio sea menor a 130.

### 3- Apoyan estos datos la hipotesis de que el consumo promedio es menor de 130 megavatios-hora
### en aquellas horas en las que la temperatura es alta? Realiza un diagrama de cajas de la variable
### consumo para cada una de las temperaturas consideradas y comenta los resultados.
	##filtrar temperatura en acero_tempAlta
	acero_tempAlta <- subset(acero2, subset=temperatura=="Alta") 
	##SE DEBE REPETIR LA NORMALIDAD
	normalityTest(~consumo, test="shapiro.test", data=acero_tempAlta) 
	#Se usa el test t para una muestra  H0 : µ ≥ 130 frente a la alternativa H1 : µ < 130 
	with(acero_tempAlta, (t.test(consumo, alternative='less', mu=130.0, conf.level=.95))) 
	# Como el p-valor<<< alpha, se rechaza la hipotesis nula. Por lo tanto hay evidencias a un 95%
	# para afirmar que el consumo promedio es menor de 130 megavatios-hora.

	#Para el diagrama de cajas es necesario volver a seleccionar el conjunto de datos total.
	Boxplot(consumo~temperatura, data=acero2, id=list(method="y"))
	## El diagrama muestra como la media se encuentra por debajo de 130 megavatios-hora cuando
	## la temperatura es alta.

### 4- Apoyan estos datos la hipotesis de que el porcentaje de veces que se usa la linea A es mayor
### del 20%?
	##Para hacer un contraste de hipotesis se necesita una variable binaria.
	# Se agrupan B y C en una sola variable no A
	acero2 <- within(acero2, {
  	AoNoA <- Recode(linea, '"A" = 1; "B" = 2; "C" = 2', as.factor=TRUE)
	})
	#Se usa test de proporciones para una muestra  H0: p  ≤ 0.20 frente a la alternativa H1: p > 0.20, 
	local({
  	.Table <- xtabs(~ AoNoA , data= acero2 )
  	cat("\nFrequency counts (test is for first level):\n")
  	print(.Table)
  	binom.test(rbind(.Table), alternative='greater', p=.20, conf.level=.95)
	})

	# Como p-valor=0.0004947<<< alpha, se rechaza la hipotesis nula. Se puede afirmar que es mayor 
	## del 20%

### 5- La compra del sistema de sobrecalentamiento no ha sido rentable si, en general, este es usado
### menos del 40% de las veces. Considerando los datos como una muestra aleatoria del comportamiento
### de esta empresa respecto a las variables en estudio, permiten concluir que la compra del sistema
### no ha sido rentable?
	#Si se denota p = Pr(OFF), con el procedimiento Test de proporciones para una muestra contrastaremos  
	#H0 : p ≤ 0.6 frente a la alternativa H1 : p > 0.6, puesto que OFF < ON  
 
	library(abind, pos=16)
	local({
  	.Table <- xtabs(~ sistema , data= acero2 )
  	cat("\nFrequency counts (test is for first level):\n")
  	print(.Table)
  	binom.test(rbind(.Table), alternative='greater', p=.60, conf.level=.95)
	})

	# Como p-valor 0.09857<<< alpha, no se rechaza la hipotesis nula. No hay evidencias suficientes para
	# afirmar que el sistema no es rentable.

### 6- Se quiere comparar el consumo promedio con el sistema de deteccion de sobrecalentamiento ON/OFF
	## Que test seria adecuando para ello? 
		#Los datos corresponden a 2 muestras independientes X e Y
		#primero se comprueba normalidad
		normalityTest(consumo ~ sistema, test="shapiro.test", data=acero2) 
		#Como ambos p-valores son mayores que los niveles de significación habituales, se pueden suponer
		#muestras procedentes de poblaciones normales. 
		#Se comprueba la varianza con Test F para dos varianzas 
		with(acero2, tapply(consumo, sistema,  var, na.rm=TRUE))
		var.test(consumo ~ sistema, alternative='two.sided', conf.level=.95, data=acero2)
		#Puesto que el p-valor>alpha, se puede suponer que las dos poblaciones normales tienen varianzas iguales.
		#De todo lo anterior se deduce que el test adecuado es el Test t para muestras independientes, 
		#con la opción varianzas iguales activada. 
	## Si la hipotesis alternativa en el contraste es que el consumo medio con el sistema OFF
	## es mayor que con el sistema ON,cual es el p-valor asociado?conclusion?
		t.test(consumo~sistema, alternative='greater', conf.level=.95, var.equal=TRUE, data=acero2)
		# Hay evidencias para afirmar al 95% que el consumo medio con el sistema apagado es significativamente 
		# mayor que con el sistema encendido
	
### 7- Se quiere comparar la produccion promedio de colada continua y del convertidor de acero
### considerando solo los datos de las horas en las que hubo produccion.
	##Que test seria el adecuado para comparar ambas producciones promedio?
		#Muestras pareadas 
		acero2_prccyca <- subset(acero2, subset=pr.cc>0 & pr.ca>0) 
		#Se obtiene la variable diferencia ya que estan relacionadas
		#medias, datos relacionados, se seleccionan las variables (resta primera menos segunda aka pr.cc, pr.ca)
		acero2_prccyca$diferencia <- with(acero2_prccyca, pr.ca - pr.cc) 
		#acero2$diferencia <- with(acero2, pr.cc- pr.ca)
		#Se hace un test de normalidad sobre esa variable
		normalityTest(~diferencia, test="shapiro.test", data=acero2_prccyca)
		#Como sale normal (p-valor = 0.6363), utilizaremos el Test t para datos relacionados.
	## Si la hipotesis alternativa en el constraste es que la produccion media de colada continua 
	## es mayor que la producción media del convertidor de acero , cual es el p-valor asociado?
		with(acero2_prccyca, (t.test(pr.cc, pr.ca, alternative='greater', conf.level=.95, paired=TRUE)))  
		# Como p-valor<<<alpha,se puede afirmar que la producción media de cc es mayor que la de ca. 

## 8- Realiza un contraste para determinar si el porcentaje de horas que el sistema de deteccion de 
## sobrecalentamiento esta apagado es mayor en la linea A que en la linea B. Cual es el p-valor asociado?
	# solo se puede hacer con variables dicotomicas asi que hay que modificar los datos
	ejercicio8 <- subset(acero2, subset=linea!="C")
	#borrar c (la guarda asi que hay que borrar las que no se usan) con descartar niveles sin uso de variable
	ejercicio8 <- within(ejercicio8, {
 	linea <- droplevels(linea) 
	})
	#Si p = Pr(OFF), contrastaremos H0: pA  ≤ pB frente a la alternativa H1: pA  > pB con el Test para 
	#proporciones para dos muestras.
	local({  .Table <- xtabs(~linea+sistema, data=ejercicio8)
  	cat("\nPercentage table:\n")
  	print(rowPercents(.Table))
 	 prop.test(.Table, alternative='greater', conf.level=.95, correct=FALSE)
	})
	#Como el p-valor es 0.6747, no hay evidencias significativas de que el porcentaje de horas que el sistema 
	#de detección de sobrecalentamiento está apagado sea mayor en la línea A que en la B. 
	
#es pareada cuando son variables distintas 

