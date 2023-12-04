#Usar el Bootstrap para estimar el error estandar de la mediana
given data X = (X(1), ..., X(n)):
  T <- median(X)
Tboot <- vector of length B
for(i in 1:B){
  Xstar <- sample of size n from X (with replacement)
  Tboot[i] <- median(Xstar)
}
se <- sqrt(variance(Tboot))

