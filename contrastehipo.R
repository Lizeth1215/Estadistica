# ejemplo del aditivo
# la hipótesis nula es que la media de rendimiento con aditivo es menor o igual a 25 (no tiene 
# efecto el aditivo)
# la hipótesis alternativa es que la media de rendimiento con aditivo es mayor a 25
# se considera que el efecto potencial del aditivo es sobre la media del rendimiento
# pero que no afecta a la dispersióin del rendimiento, tal dispersión medida por 
# la desviación estándar es 2.4
#Y~N[m, (sqrt(2.4))]
#Y: el rendimiento de los carros con la nueva gasolina.
#25 es el valor medio de la gasolina actualmente usada.
#HO: m<=25
#H1: m>25


mu0<-25
sigma<-2.4
# una muestra de 30 carros se les administra el aditivo
n<-30
# después de transitar registran un valor promedio de 26.3
Ybarra_obs<-26.3

# el criterio de contraste selecionado es si el promedio muestral es grande 
# se rechace H0 

###### enfoque de alfa fijo y entonces estudiar la potencia del contraste
# En este caso, el criterio:  si el promedio muestral es más que un valor crítico
# se rechaza H0 
# el valor crítico se determina con el fin de garantizar que el error tipo I 
# tenga una porbabilidad (fija) de alfa y se estudie la potencia respectiva
alfa<-0.05 # por alguna razón
Z_alfa<-qnorm(1-alfa)
# valor crítico es, en la escala de YZ
Ybarra_star<-(sigma/sqrt(n))*Z_alfa+mu0
Ybarra_star
#dado este valor crítico en la escala de Y
# vamos a estudiar la función potencia asociada
potencia<-c()
mua<-seq(25,35,0.1)# valores de mu en H0 y cuando el aditivo sí funciona
for (i in 1:101)
	{
		z<-((Ybarra_star-mua[i])/sigma)*sqrt(n)
		potencia[i]<-1-pnorm(z)

	}
plot(mua,potencia)
cbind(mua,potencia)


#####enfoque del p - valor
# el valor observado en el experimento del promedio de los carros es 26.3
#entonces el p-valor es
pvalor<-1-pnorm(((26.3-25)/sigma)*sqrt(n))
pvalor
# la probabilidad de observar el resultado del experimento es altamente improbable 
# suponiendo que mu con aditivo es 25 
# lo improbable no es imposible, pero sí sugiere este p-valor que sería raro observarse 
