library(MASS)
nerve<-read.csv("nervios.csv")
tpoesp<-nerve$tiempo
par(mfrow=c(1,1))
n<-length(tpoesp)
truehist(tpoesp)
truehist(tpoesp,density=TRUE)### histograma
exp3<-function(tpoesp) 3*exp(-3*tpoesp)### asumiendo como exponencial 3
curve(exp3, 0,1.2,add=TRUE)
### descripción de la muestra: ver a los datos y detectar patrones dominantes
### y comportamientos atípicos en los datos
fivenum(tpoesp)
### min, cuartil 1 Q1 (acumula 25%), mediana X tilde (acumula el 50$%) cuartil 3 Q3(acumula 75%), máximo
mean(tpoesp)
### promedio
boxplot(tpoesp)
### muestra adyancente inferior, Q1, X tilde, Q3, adyacente superior y por ende atípicos
median(tpoesp)

### five y median y mean nos dan idea de d?nde se olcalizan los datos
### otra medida importante de un conjunto es c?mo se dispersan
var(tpoesp)
sd(tpoesp)
mad(tpoesp)

#vamos a computar la función de distribución empírica

nerve_ecdf<-ecdf(tpoesp)
plot(nerve_ecdf, verticals=TRUE, do.points=FALSE, main="ditsribucipon emp?rica con intervalo de confianza al 95% de los tiempos de esera en nervios",
     xlab="tiempo de espera en segundos", ylab=" funci?n de distribuci?n emp?rica",
     col="black")
# para su intervalo de confianza
alpha <-0.05
en <- sqrt(log(2/alpha)/(2*n))
L_DKW <- pmax(nerve_ecdf(tpoesp)-en,0)
U_DKW <- pmin(nerve_ecdf(tpoesp)+en,1)
points(sort(tpoesp), L_DKW[order(tpoesp)], "l", col="red")
points(sort(tpoesp), U_DKW[order(tpoesp)], "l", col="red")
lines(c(.5,.5), c(0,1))