library(fitdistrplus)

datosnor<-c(3.23, -2.50, 1.88, -0.68, 4.43, 0.17,
1.03, -0.07, -0.01, 0.76, 1.76, 3.18,
0.33, -0.31, 0.30, -0.61, 1.52, 5.43,
1.54, 2.28, 0.42, 2.33, -1.03, 4.00, 0.39)

ajustenor<- fitdist(data = datosnor, 
                      distr = 'norm', 
                      method = 'mle')
