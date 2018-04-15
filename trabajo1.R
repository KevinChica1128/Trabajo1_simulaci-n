#-----------------------------------#
# Kevin Steven Garcia Chica
# Cesar A. Saavedra 
# Simulacion Estadistica Trabajo 1
#-----------------------------------#

nsim=10000

#-----------------------------------#
#Distribucion Poisson:
#-----------------------------------#
P <- rpois(nsim, 1.99)
q <- table(P)


U <- runif(nsim,0,1)


plot(q, freq=T)
plot(density(q))

#-----------------------------------#
#Distribucion Logistic:
#-----------------------------------#
L <- rlogis(nsim,7.485,0.01771)
L2 <- rlogis(nsim,7.482,0.0256)

U <- runif(nsim,0,1)
# Transformacion #
X = log((U)/(1-U))
l <- seq(-8,8, by=0.001)

par(mfrow=c(1,2))
hist((L), main="Logic Tradicional")
hist((X), main="Logic Transformacion", freq=F)
lines(l, dlogis(l,1,3))

