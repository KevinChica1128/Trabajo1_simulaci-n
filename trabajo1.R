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


E <- rexp(nsim,1.99)
# Transformacion #
Y = -(1/1.99)*(log(E))

plot(P)
plot(Y)

#-----------------------------------#
#Distribucion Logistic:
#-----------------------------------#
L <- rlogis(nsim, 3,4)

U <- runif(nsim,0,1)
# Transformacion #
X = log((U)/(1-U))


par(mfrow=c(1,2))
hist((L), main="Logic Tradicional")
hist((X), main="Logic Transformacion", freq=F)



       