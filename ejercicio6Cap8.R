library(MASS)
library(ggplot2)
library(tidyverse)
S<-100 # de simulaciones
B<-1000 #n?mero de muestras bootstrap
muestra<-rnorm(100,5,1)
#x<-exp(muestra)
promedio<-sum(muestra)/100
promedio
TF<-exp(promedio)
  TFb<-c()
  for(i in 1:B)
  {
    xi<-sample(muestra,100,replace=TRUE)
    promedio1<-mean(xi)
    
    #vari<-(sum((xi-promedio)**2))/50
    TFb[i]=exp(promedio1)
    TFb[i]<-TFb[i]
  }
  se=sqrt(var(TFb))
  Normal=c(TF-(qnorm(.975)*se),TF+(qnorm(.975)*se))
  Pivotal=c(2*TF-quantile(TFb,.975),2*TF-quantile(TFb,.025))
  Percentil=c(quantile(TFb,0.025),quantile(TFb,0.975))

sprintf("El intervalo de confianza normal es (%.6s,%.6s)",Normal[1],Normal[2])
sprintf("El intervalo de confianza pivotal es (%.6s,%.6s)",Pivotal[1],Pivotal[2])
sprintf("El intervalo de confianza Percentil es (%.6s,%.6s)",Percentil[1],Percentil[2])
#x<-exp(muestra)
#parametro bootstrap
promedio<-sum(muestra)/100
promedio
TF<-exp(promedio)
TFb1<-c()
for(i in 1:B){
  xx<-rnorm(100,promedio,1)
  promedio1<-sum(xx)/100
  TFb1[i]=exp(promedio1)
  TFb1[i]<-TFb1[i]
}
se=sqrt(var(TFb1))
Normal=c(TF-(qnorm(.975)*se),TF+(qnorm(.975)*se))
Pivotal=c(2*TF-quantile(TFb1,.975),2*TF-quantile(TFb1,.025))
Percentil=c(quantile(TFb1,0.025),quantile(TFb1,0.975))
sprintf("El intervalo de confianza normal es (%.6s,%.6s)",Normal[1],Normal[2])
sprintf("El intervalo de confianza pivotal es (%.6s,%.6s)",Pivotal[1],Pivotal[2])
sprintf("El intervalo de confianza Percentil es (%.6s,%.6s)",Percentil[1],Percentil[2])
par(mfrow = c(1, 2))
hist(TFb,main = "Remuestreo")
hist(TFb1,main="Nuevas muestras")

ggplot() +
  geom_histogram(aes(x =TFb))
ggplot()+
  geom_histogram(aes(x =TFb1,color='red'))
