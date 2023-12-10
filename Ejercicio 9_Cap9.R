###Ejercicio 9 capitulo 9. 
#Bootstrap no parametrico y parametrico

rm(list=ls())
X<-rnorm(100,5,1) #conjunto de 100 datos obtenidos de una normal(5,1)
n<-length(X)
t<-exp(5) #theta=exp(media)
t
thetah<-exp(mean(X)) #estimador de theta dada la muestra X
thetah
#BOOTSTRAP NO PARAMETRICO para el estimador theta
B<-1000 #tamaño de muestra bootstrap
TFg<-c()
for(i in 1:B)
{
  Xi<-sample(X,n,replace=T)
  TFg[i]=exp(mean(Xi))
}
var(TFg)
sesgo<-mean(TFg)-t
sesgo
mean(TFg)
se=sqrt(var(TFg))
se # error estandar de theta gorro, aproximacion Bootstrap
Normal=c(thetah-(qnorm(.975)*se),thetah+(qnorm(.975)*se))
Normal
Percentil=c(quantile(TFg,0.025),quantile(TFg,0.975))
# intervalo bootstrap
Percentil
hist(TFg,freq=FALSE)#podemos decir que el histograma es bastante simetrico, normal
x<-sort(TFg)
curve(dlnorm(x,5,1/10), add=TRUE)#distribucion teorica
#Cuando comparo boostrap no parametrico contra la teorica vemos que hya priblemas. La distribucion no parametrica
#Esta nun poco corrida. Hay sesgooo, y una forma de corregir ese sesgo es mediante la BCa
#Esyo esta sesgado. lo Teorico deberia estar mas centrado en esta distribucion 

### hay ciertos problemas de sesgo!! de la distribucion real versus 
### la de bootstrap no parametrico


# BCa
#Lo que hace el BCa es corregir el sesgo
library(bcaboot)
x<-rnorm(100,5,1) #conjunto de 100 datos obtenidos de una normal(5,1)
rfun <- function(x) {exp(mean(x))}
bcajack(x = x, B = 2000, func = rfun, verbose = FALSE) 
#Caundo aplico el BCa si recupera mas la teorica, esta más centrado en la teorica, entonces
#corrige el sesgo de los intervalos de bootstrap 
## que corrige el sesgo mostrado anteriormente


#Con bootstrap parametrico se corrige el sesgo de las lineas anteriores
rm(list=ls())
X<-rnorm(100,5,1) #conjunto de 100 datos obtenidos de una normal(5,1)
n<-length(X)
t<-exp(5) #theta=exp(media)
t
thetah<-exp(mean(X)) #estimador de theta dada la muestra X
thetah
#BOOTSTRAP PARAMETRICO para el estimador theta
B<-1000 #tamaño de muestra bootstrap
TFg<-c()
for(i in 1:B)
{
  Xi<-rnorm(length(X),mean(X),sd(X))
  TFg[i]=exp(mean(Xi))
}
mean(TFg)
var(TFg)
se=sqrt(var(TFg))
se # error estandar de theta gorro, aproximacion Bootstrap
sesgo<-mean(TFg)-t
sesgo
Normal=c(thetah-(qnorm(.975)*se),thetah+(qnorm(.975)*se))
Normal
Percentil=c(quantile(TFg,0.025),quantile(TFg,0.975))
# intervalo bootstrap
Percentil

hist(TFg,freq=FALSE)
x<-sort(TFg)
curve(dlnorm(x,5,1/10), add=TRUE)
#Aqui el bootstrap parametrico tambien esta bastante mal. En este caso resulta mejor
#usar el BCa