muestra<-runif(50,0,1)
X1<-max(muestra)
X1
TFb<-c()
for(i in 1:B)
{
  xi<-sample(muestra,50,replace=TRUE)
  
  
  #vari<-(sum((xi-promedio)**2))/50
  TFb[i]=max(xi)
  TFb[i]<-TFb[i]
}
se=sqrt(var(TFb))
Normal=c(X1-(qnorm(.975)*se),X1+(qnorm(.975)*se))
Pivotal=c(2*X1-quantile(TFb,.975),2*X1-quantile(TFb,.025))
Percentil=c(quantile(TFb,0.025),quantile(TFb,0.975))

sprintf("El intervalo de confianza normal es (%.6s,%.6s)",Normal[1],Normal[2])
sprintf("El intervalo de confianza pivotal es (%.6s,%.6s)",Pivotal[1],Pivotal[2])
sprintf("El intervalo de confianza Percentil es (%.6s,%.6s)",Percentil[1],Percentil[2])
