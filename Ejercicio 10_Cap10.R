###Ejercicio 10 del capitulo 9
#Bootstrap no parametrico y parametrico
rm(list=ls())
x<-runif(50,0,1)#50 valores aleatorios de una variable uniforme(0,1)
B<-1000 #tamaño de la muestra bootstrap
xmax<-max(x) #valor maximo de la muestra original de la v.a. x
xmax
xmaxb<-c()# donde depositar el maximo de cada muestra bootstrap
for (i in 1:B)
{
  xi<-sample(x,50, replace=TRUE)
  xmaxb[i]=max(xi)
}

se<-sqrt(var(xmaxb))
se
mean(xmaxb)

u<-seq(0,1,0.01)
dxmax <- 50*u^49 # densidad teorica del maximo de una muestra de Uniforme[0,1]
library(MASS)
hist(xmaxb, xlab="X(n)", main="histograma de maximos por bootstrap", freq=FALSE, xlim=c(0,1))
lines(u,dxmax)
#En este caso, con el bootstrap no parametrico, se concentra en el maximo observado y sale una barra que se puede ver en el histograma
#En el valor maximo observado

#### muy pobre la aproximaciOn de la distribuciOn bootstrap a la analitica
### siendo no parametrico, nunca en muestras bootstrap habra valores mayores al maximo de la muestra
### original, situacion no deseable por baja representatividad de todo el rango
### de la Uniforme 0,1

#BCa
#Muestra que los histogramas de comnfianza que hemos visto, los tres, el pivital se descarta, el que queda
#mejor es el normal. Lo que hace el BCa respecto de los intervalos es que los ajusta, pues tiene un sesgo hacia abajo
#entonces, corrige el intervalo para el maximo de la muestra con el 95% de confianza
library(bcaboot)
rfun <- function(x) {max(x)}
bcajack(x = x, B = 1000, func = rfun, verbose = FALSE) 
### el problema NO es corregido con el BCa...



#Con bootstrap parametrico
rm(list=ls())
x<-runif(50,0,1)#50 valores aleatorios de una variable uniforme(0,1)
B<-1000 #tamaño de muestra bootstrap
xmax<-max(x) #valor maximo de la muestra original de la v.a. x
xmax
xmaxb<-c()# donde depositar el maximo de cada muestra bootstrap
for (i in 1:B)
{
  uniforme<-runif(50,0,xmax)#Aquí tengo que poner el a y el b estimado, en este caso
  #a=0, pero b es el maximo de la muestra. Voy a remuestrar del 0 al xmax, que va a ser el valor maximo
  #Se remuestrea del 0 al max de la muestra original
  xmaxb[i]=max(uniforme)
}

se<-sqrt(var(xmaxb))
se
mean(xmaxb)

u<-seq(0,1,0.01)
dxmax <- 50*u^49 # densidad teorica del maximo de una muestra de Uniforme[0,1]
library(MASS)
hist(xmaxb, xlab="X(n)", main="histograma de maximos por bootstrap parametrico", freq=FALSE, xlim=c(0,1))
lines(u,dxmax)#ley teorica, se pega a los valores obtenidos
#Notemos aqui, que la ley ley teorica, que es la linea esa, se pega a la distribucion de valores parametricos 
#cosa que no sucede con el no parametrico, pues el histograma de esos valores esta concentrado en un solo valor
#En este caso, en el caso de la uniforme, es mucho mejor el boostrao PARAMETRICO