#Ejercicio 3 inciso c). Encontrar el estimador de tao y 
#encontrar el error estandar usando bootstrap parametrico

x<-c(3.23, -2.50, 1.88, -0.68, 4.43,
     0.17, 1.03, -0.07, -0.01, 0.76, 1.76,
     3.18, 0.33, -0.31, 0.30, -0.61, 1.52,
     5.43, 1.54, 2.28, 0.42, 2.33, -1.03,
     4.00, 0.39)
mle<-c()
for(i in 1:2000){
  xb<-rnorm(length(x),mean(x),sd(x))
  mle[i]<-mean(xb)+sd(xb)*1.64#estimador puntual de tao, tao gorro
}
#datos de la poblacion con distribucion normal
mean(x)
var(x)
sd(x)

#Error estandar usando bootstrap parametrico
se<-sqrt(var(mle))
se
var(mle)


