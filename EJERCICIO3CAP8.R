S<-100 # de simulaciones
B<-1000 #n?mero de muestras bootstrap
TFP=(qt(.75,3)-qt(.25,3))/1.34 # el par?mtero de inter?s
longnormal<-0
longpivotal<-0
longpercentil<-0
countn<-0
countper<-0
countpiv<-0
for (s in 1:S){
  x<-rt(25,3)
  TFm=(quantile(x,.75)-quantile(x,.25))/1.34
  TFb<-c()
  for(i in 1:B)
  {
    xi<-sample(x,25,replace=TRUE)
    TFb[i]=(quantile(xi,.75)-quantile(xi,.25))/1.34
    TFb[i]<-TFb[i]
    
  }
  se=sqrt(var(TFb))
  Normal=c(TFm-(qnorm(.975)*se),TFm+(qnorm(.975)*se))
  Pivotal=c(2*TFm-quantile(TFb,.975),2*TFm-quantile(TFb,.025))
  Percentil=c(quantile(TFb,0.025),quantile(TFb,0.975))
  TFm #estimaci?n a partir de muestra original
  se # error estandar de TFm v?a aproximaci?n Bootstrap
  # c?lculo de longitudes de cada intervalo bootstrap
  longnormal<-longnormal+Normal[2]-Normal[1]
  longpivotal<-longpivotal+Pivotal[2]-Pivotal[1]
  longpercentil<-longpercentil+Percentil[2]-Percentil[1]
  #c?lculo de la cobertura de cada intervalo bootstrap
  if (Normal[1]<TFP & Normal[2]>TFP) countn<-countn+1
  if (Percentil[1]<TFP & Percentil[2]>TFP) countper<-countper+1
  if (Pivotal[1]<TFP & Pivotal[2]>TFP) countpiv<-countpiv+1
}
#longitudes promedio
longnormal/S
longpercentil/S
longpivotal/S
#Coberturas
countn/S
countper/S
countpiv/S
