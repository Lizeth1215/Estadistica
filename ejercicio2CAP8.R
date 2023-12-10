muestra<-rnorm(50)
muestra
promedio<-mean(muestra)
var(muestra)
vari<-(sum((muestra-promedio)**2))/50
n<-50
vari
  x<-sum((muestra - promedio)**3 / (n * vari**(3/2)))
x
S<-100 # de simulaciones
B<-1000 #n?mero de muestras bootstrap
for (s in 1:S){
  muestra<-rnorm(50)
  x<-exp(muestra)
  promedio<-mean(x)
  var(x)
  vari<-(sum((x-promedio)**2))/50
  TF<-sum((x - promedio)**3 / (n * vari**(3/2)))
  TFb<-c()
  for(i in 1:B)
  {
    xi<-sample(x,50,replace=TRUE)#remuestrear y elevar o elevar y remuestrear
    promedio<-mean(xi)
    vari<-(sum((xi-promedio)**2))/50
    TFb[i]=sum((xi - promedio)**3 / (n * vari**(3/2)))
    TFb[i]<-TFb[i]
  }
  se=sqrt(var(TFb))
  Normal=c(TF-(qnorm(.975)*se),TF+(qnorm(.975)*se))
  Pivotal=c(2*TF-quantile(TFb,.975),2*TF-quantile(TFb,.025))
  Percentil=c(quantile(TFb,0.025),quantile(TFb,0.975))
}
sprintf("El intervalo de confianza normal es (%.6s,%.6s)",Normal[1],Normal[2])

sprintf("El intervalo de confianza pivotal es (%.6s,%.6s)",Pivotal[1],Pivotal[2])

sprintf("El intervalo de confianza Percentil es (%.6s,%.6s)",Percentil[1],Percentil[2])
