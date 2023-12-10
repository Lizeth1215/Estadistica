library(MASS)
Estudiante<-read.csv("C:/Users/andre/Downloads/Estudiante.csv")
Hours_Studied<-Estudiante$Hours_Studied
n<-length(Hours_Studied)

truehist(Hours_Studied)
########################plot(main ="Hours_Studied")
fivenum(Hours_Studied)
### min, cuartil 1 Q1 (acumula 25%), mediana X tilde (acumula el 50$%), cuartil 3 Q3(acumula 75%), máximo
mean(Hours_Studied)### promedio
boxplot(Hours_Studied)
### muestra adyancente inferior, Q1, X tilde, Q3, adyacente superior y por ende atípicos
median(Hours_Studied)

var(Hours_Studied)
sd(Hours_Studied)
mad(Hours_Studied)

#función de distribución empírica
nerve_ecdf<-ecdf(Hours_Studied)
plot(nerve_ecdf, verticals=TRUE, do.points=FALSE, main="Estudiantes",
xlab="Hours Studied", ylab="Numero de estudiantes",col="black")
     
#intervalo de confianza
alpha <-0.05
en <- sqrt(log(2/alpha)/(2*n))
L_DKW <- pmax(nerve_ecdf(Hours_Studied)-en,0)
U_DKW <- pmin(nerve_ecdf(Hours_Studied)+en,1)
points(sort(Hours_Studied), L_DKW[order(Hours_Studied)], "l", col="red")
points(sort(Hours_Studied), U_DKW[order(Hours_Studied)], "l", col="red")
     
