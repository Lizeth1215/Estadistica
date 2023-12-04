#Este codigo esta bien!!
#Ejercicio 2. capitulo 9 inciso d)
#Encuentre el MSE de T gorro mediante simulacion
a<-1
b<-3
n<-10
t<-c()
for(i in 1:2000)
{
  uniforme<-runif(n,a,b)
  xmin<-min(uniforme)
  xmax<-max(uniforme)
  t[i]<-(xmin+xmax)/2 #estimador puntual de tao, tao gorro 
}
mean(t)
c<-(a+b)/2
v<-var(t)
sesgo<-mean(t)-c
sesgo
mse<-sesgo^2+v
mse
sd(t)
