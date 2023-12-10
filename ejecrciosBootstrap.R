####################Ejercicio 8.1

datosexamen<-read.csv("lsatgap.csv")
r<-cor(datosexamen[,1],datosexamen[,2])
r
n<-nrow(datosexamen)

B<-1000          
  
indice<-1:n
corb<-c()

for(i in 1:B)
{
 indiceB<-sample(indice, replace=TRUE)
  datos<-datosexamen[indiceB,]
  corb[i]<-cor(datos[,1], datos[,2])
}

se=sqrt(var(corb))  #error est?ndar v?a bootstrap
se
#Intervalos de confianza bootstrap 
Normal=c(r-(2*se),r+(2*se))    
Pivotal=c(2*r-quantile(corb,.975),2*r-quantile(corb,.025))
Percentil=c(quantile(corb,.025),quantile(corb,.975))
Normal 
Percentil
Pivotal      
hist(corb, freq=FALSE)  #Histograma de las muestras bootstrap
#viendo el histrograma es mejor usar el método
# del pivote o el percentil bootstrap
plot(datosexamen[,1],datosexamen[,2], xlab="LSAT", ylab="GPA", col="red") # grafica de
# se precibe una relaci?n cuasi lineal entre las
# dos calificaciones r= 0.77

library(bcaboot)

Xy <- datosexamen
rfun <- function(Xy) {
  y <- Xy[, 1]
  X <- Xy[, 2]
  cor(y,X)
}
set.seed(1234)
bcajack(x = Xy, B = 1000, func = rfun,m=5, verbose = FALSE)






#################Ejercicio 8.2
library(e1071)

n<-500
TFP<-(exp(1)+2)*sqrt(exp(1)-1)
TFP

   ### valor poblacional del coeficienbte de asimetría
### de una variable lognormal (0,1)

S<-100 # de simulaciones
B<-2000 #n?mero de muestras bootstrap
### longitudes 
longnormal<-0
longpivotal<-0
longpercentil<-0
### coberturas
countn<-0
countper<-0
countpiv<-0
for (s in 1:S){
       Y<-rnorm(n)
       x<-exp(Y)### lognormal (0,1)
       TFm<-skewness(x,type=1)## cómputo del coeficiente de asimetría muestral
       TFb<-c()
       for(i in 1:B)
       {
         Xi<-sample(x,n,replace=TRUE)
         TFb[i]=skewness(Xi, type=1)
       }
       se=sqrt(var(TFb))
       Normal=c(TFm-(qnorm(.975)*se),TFm+(qnorm(.975)*se))
       Pivotal=c(2*TFm-quantile(TFb,.975),2*TFm-quantile(TFb,.025))
       Percentil=c(quantile(TFb,.025),quantile(TFb,.975))
       se # error est?ndar de TF v?a aproximaci?n Bootstrap
       # intervalos bootstrap
       longnormal<-longnormal+Normal[2]-Normal[1]
       longpivotal<-longpivotal+Pivotal[2]-Pivotal[1]
       longpercentil<-longpercentil+Percentil[2]-Percentil[1]
       #cálculo de la cobertura de cada intervalo bootstrap
       if (Normal[1]<TFP & Normal[2]>TFP) countn<-countn+1
       if (Percentil[1]<TFP & Percentil[2]>TFP) countper<-countper+1
       if (Pivotal[1]<TFP & Pivotal[2]>TFP) countpiv<-countpiv+1
}
#longitudes promedio
longnormal/S
longpivotal/S
longpercentil/S
#Coberturas
countn/S
countper/S
countpiv/S


#### al parecer los intervalos bootstrap no paramétricos hacen mal trabajo en términos de la cobertura

### corrigiendo problemas de sesgo y de rapidez de la 
### la convergencia a cobertura nominal

library(bcaboot)
n<-50
Y<-rnorm(n)
X<-exp(Y)
rfun <- function(X) {skewness(X, type=1)}
set.seed(1234)
int<-bcajack(x = X, B = 2000, func = rfun, verbose = FALSE)   
int$lims[1]
int$lims[2]
### que NO lucen muy prometedores en este caso lognaormal(0,1)
### Pero si en lugar de lognormal(0,1), fuera lognormal(0, 0.3)
### que ya no es de colas pesadas
### bootstrap no paramétrico por el método de percentil
### cumple bien en cobertura

 #### bootstrap paramétrico

library(e1071)

n<-50 
### es muy influyente el tamaño de muestra aun en el caso bootstrap paramétrico
### compare n=50 con n=500
TFP<-(exp(1)+2)*sqrt(exp(1)-1)
TFP

### valor poblacional del coeficienbte de asimetría
### de una variable lognormal (0,1)

S<-100 # de simulaciones
B<-2000 #número de muestras bootstrap
### longitudes 
longnormal<-0
longpivotal<-0
longpercentil<-0
### coberturas
countn<-0
countper<-0
countpiv<-0
for (s in 1:S){
  Y<-rnorm(n)
  x<-exp(Y)
  TFm<-skewness(x,type=1)## cómputo del coeficiente de asimetría muestral
  TFb<-c()
  library(fitdistrplus)
  mlepara<-mledist(x, "lnorm")
  muloge<-mlepara$estimate[1]
  sdloge<-mlepara$estimate[2]
  for(i in 1:B)
  {
    Xi<-rlnorm(n, meanlog = muloge, sdlog = sdloge)
    TFb[i]=skewness(Xi, type=1)
  }
  se=sqrt(var(TFb))
  Normal=c(TFm-(qnorm(.975)*se),TFm+(qnorm(.975)*se))
  Pivotal=c(2*TFm-quantile(TFb,.975),2*TFm-quantile(TFb,.025))
  Percentil=c(quantile(TFb,.025),quantile(TFb,.975))
  se # error est?ndar de TF v?a aproximaci?n Bootstrap
  # intervalos bootstrap
  longnormal<-longnormal+Normal[2]-Normal[1]
  longpivotal<-longpivotal+Pivotal[2]-Pivotal[1]
  longpercentil<-longpercentil+Percentil[2]-Percentil[1]
  #cálculo de la cobertura de cada intervalo bootstrap
  if (Normal[1]<TFP & Normal[2]>TFP) countn<-countn+1
  if (Percentil[1]<TFP & Percentil[2]>TFP) countper<-countper+1
  if (Pivotal[1]<TFP & Pivotal[2]>TFP) countpiv<-countpiv+1
}
#longitudes promedio
longnormal/S
longpivotal/S
longpercentil/S
#Coberturas
countn/S
countper/S
countpiv/S




       
#######################Ejercicio 8.3
       S<-100 # de simulaciones
       B<-2000 #número de muestras bootstrap
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
         TFm #estimación a partir de muestra original
         se # error estandar de TFm vía aproximación Bootstrap
         # cálculo de longitudes de cada intervalo bootstrap
         longnormal<-longnormal+Normal[2]-Normal[1]
         longpivotal<-longpivotal+Pivotal[2]-Pivotal[1]
         longpercentil<-longpercentil+Percentil[2]-Percentil[1]
         #cálculo de la cobertura de cada intervalo bootstrap
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
       
### en general en este caso es razonable la cobertura salvo el del método
### pivotal

       
library(bcaboot)
x<-rt(25,3)
rfun <- function(x) {(quantile(x,.75)-quantile(x,.25))/1.34}
bcajack(x = x, B = 2000, func = rfun, verbose = FALSE) 
       
       

######################Ejercicio 8.6
rm(list=ls())
X<-rnorm(100,5,1) #conjunto de 100 datos obtenidos de una normal(5,1)
n<-length(X)
t<-exp(5) #theta=exp(media)
thetah<-exp(mean(X)) #estimador de theta dada la muestra X
#BOOTSTRAP para el estimador theta
B<-1000 #tamaño de muestra bootstrap
TFg<-c()
for(i in 1:B)
       {
         Xi<-sample(X,n,replace=T)
         TFg[i]=exp(mean(Xi))
       }
se=sqrt(var(TFg))
se # error estandar de theta gorro vía aproximación Bootstrap
Normal=c(thetah-(qnorm(.975)*se),thetah+(qnorm(.975)*se))
Normal
Percentil=c(quantile(TFg,0.025),quantile(TFg,0.975))
# intervalo bootstrap
Percentil
hist(TFg,freq=FALSE)
x<-sort(TFg)
curve(dlnorm(x,5,1/10), add=TRUE)
### hay ciertos problemas de sesgo de la distribución real versus 
### la de bootstrap

library(bcaboot)
x<-rnorm(100,5,1) #conjunto de 100 datos obtenidos de una normal(5,1)
rfun <- function(x) {exp(mean(x))}
bcajack(x = x, B = 2000, func = rfun, verbose = FALSE) 

## que corrige el sesgo mostrado anteriormente


     
#########################Ejercicio 8.7
rm(list=ls())
x<-runif(50,0,1)#50 valores aleatorios de una variable uniforme(0,1)
B<-1000 #tamaño de muestra bootstrap
xmax<-max(x) #valor máximo de la muestra original de la v.a. x
xmaxb<-c()# donde depositar el máximo de cada muestra bootstrap
for (i in 1:B)
       {
         xi<-sample(x,50, replace=TRUE)
         xmaxb[i]=max(xi)
       }
se<-sqrt(var(xmaxb))
u<-seq(0,1,0.01)
dxmax <- 50*u^49 # densidad teórica del máximo de una muestra de Uniforme[0,1]
library(MASS)
hist(xmaxb, xlab="X(n)", main="histograma de máximos por bootstrap", freq=FALSE, xlim=c(0,1))
lines(u,dxmax)
       
#### muy pobre la aproximación de la distribución bootstrap a la analítica
### siendo no paramétrico, nunca en muestras bootstrap habrá valores mayores al máximo de la muestra
### original, situación no deseable por baja representatividad de todo el rango
### de la Uniforme 0,1
library(bcaboot)

rfun <- function(x) {max(x)}

bcajack(x = x, B = 1000, func = rfun, verbose = FALSE) 
### el problema NO es corregido con el BCa...

### explorar con bootstrap paramétrico
       
       
       
       
       
       
       
       
       
#################### Ejemplo 8.7

old_placebo<-c(8406,2342,8187,8459,4795,3516,4796,10238) #datos del old_placebo
new_old<-c(-1200,2601,-2705,1982,-1290,351,-638,-2719) #datos de New_old

z<-mean(old_placebo)  #media de old_placebo
y<-mean(new_old)   #media de new_old
th<-y/z      #theta #-0.0713 muestralmente 
th
### para ser aceptable el nuevo medicamento debe ser
### en valor abosluto menor o igual a 0.20
### estimación putual da idea de que sí es aceptable7
### pero considerando intervalos estadísticos:

n<-length(old_placebo)  #número de sujetos participantes

B<-1000   #bootstrap
Tboot<-numeric(B)

for(i in 1:B)  #simulación bootstrap 
{
  xx1<-sample(old_placebo,n,replace=T)
  xx2<-sample(new_old,n,replace=T)
  Tboot[i]<-mean(xx2)/(mean(xx1))
}
se=sqrt(var(Tboot))   #error estandar vía bootstrap no paramétrico
#intervalos de confianza bootstrap
Normal=c(th-(2*se), th+(2*se))
Pivotal=c(2*th-quantile(Tboot,.975),2*th-quantile(Tboot,.025))
Percentil=c(quantile(Tboot,.025),quantile(Tboot,.975))
Normal
Pivotal
Percentil
hist(Tboot, freq=FALSE)
#histograma de las muestras bootstrap

library(bcaboot)
Xy<-cbind(old_placebo, new_old)
rfun <- function(Xy) {mean(Xy[,2])/mean(Xy[,1])}

bcajack(x = Xy, B = 1000, func = rfun, verbose = FALSE) 
### que reafirma las dudas de que el medicamento nuevo sea aceptable
### versus el medicamento actual(viejo)
