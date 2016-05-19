# 18.mayo.2016
tarea (graficas , holt, metodos que pronostican datos)

## 18 de mayo de 2016 ##
## tendencias de holt

install.packages("fpp")
library (fpp)
install.packages("forecast")
library (forecast)
install.packages("foreing")
library (foreing)
bimbo <- read.csv("C:\\Users\\Paula\\Desktop\\bimbo.csv")
netflix <-  read.csv("C:\\Users\\Paula\\Desktop\\netflix.csv")
tsbimbo <- ts(bimbo[,2], start = 2015, frequency = 52)
tsnetflix <- ts(netflix[,2], start = 2015, frequency = 52)
plot(tsbimbo)
plot(tsnetflix)

#### BIMBO
b_ajustehl <- holt(tsbimbo,initial = "simple", h= 4)
b_ajustehe <- holt(tsbimbo,initial = "simple", exponential = T, h= 4)
b_ajusaa <- holt(tsbimbo, damped = T)
b_ajusma <-holt(tsbimbo, exponential = T, damped = T)

plot(tsbimbo, plot.conf =FALSE, main = "Precio de Acciones de Bimbo") 
lines(fitted(b_ajustehl), col = 2)
lines(fitted(b_ajustehe), col = 3)
lines(fitted(b_ajusaa), col = 4)
lines(fitted(b_ajusma), col = 5)
legend("topleft", lty = 5, col= c(1,2,3,4,5), legend = c("holt lineal", "holt exponencial", "aditiva amortiguada", "multiplicativa amortiguada"))

b_meanf <- meanf(tsbimbo,4)
b_menf_eval <- accuracy(b_meanf)
b_naive <- naive(tsbimbo,4)
b_naive_eval <- accuracy(b_naive)
b_snaive <- snaive(tsbimbo,4)
b_snaive_eval <- accuracy(b_snaive)
b_rwf <- rwf(tsbimbo,4, drift = T)
b_rwf_eval <- accuracy(b_rwf)


#### NETFLIX
n_ajustehl <- holt(tsnetflix,initial = "simple", h= 4)
n_ajustehe <- holt(tsnetflix,initial = "simple", exponential = T, h= 4)
n_ajusaa <- holt(tsnetflix, damped = T)
n_ajusma <-holt(tsnetflix, exponential = T, damped = T)

plot(tsnetflix, plot.conf =FALSE, main = "Precio de Acciones de Netflix") 
lines(fitted(n_ajustehl), col = 2)
lines(fitted(n_ajustehe), col = 3)
lines(fitted(n_ajusaa), col = 4)
lines(fitted(n_ajusma), col = 5)
legend("topleft", lty = 5, col= c(1,2,3,4,5), legend = c("holt lineal", "holt exponencial", "aditiva amortiguada", "multiplicativa amortiguada"))

n_meanf <- meanf(tsnetflix,4)
n_naive <- naive(tsnetflix,4)
n_snaive <- snaive(tsnetflix,4)
n_rwf <- rwf(tsnetflix,4, drift = T)

n_menaf_eval <- accuracy(n_meanf)
n_naive_eval <- accuracy(n_naive)
n_snaive_eval <- accuracy(n_snaive)
n_rwf_eval <- accuracy(n_rwf)
