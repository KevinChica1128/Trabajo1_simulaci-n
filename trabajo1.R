#-----------------------------------#
# Kevin Steven Garcia Chica
# Cesar A. Saavedra 
# Simulacion Estadistica Trabajo 1
#-----------------------------------#
suppressMessages(library(ggplot2))
nsim=10000
#-----------------------------------#
#Distribucion Poisson:
#-----------------------------------#
# M. tradicional #
P <- rpois(nsim, 1.99)


# Transformacion #
x=0
t=0
X <- rexp(nsim,1/1.99)
t=t+X

for (x in t) {
  
}
Y = -(1/1.99)*(log(E))

#Grafico#
barplot(table(P), xlab="Magnitud", ylab="Tasa de Terremotos", main="Probabilidad de Riesgo Sismico")
qplot(P, main="Distribucion Poisson para Riesgo Sismico", 
      xlab="Magnitud", ylab="Tasa de Terremotos", col=I("black"),fill=I("red")) 




#-----------------------------------#
#Distribucion Logistic:
#-----------------------------------#
# M. tradicional #
L <- rlogis(nsim, 3,4)

# Transformacion #
a=3
b=4
U <- runif(nsim,0,1)
X = a + b*log((U)/(1-U))

datos <- data.frame(L,X)

#Grafico#
qplot(L, main="Distribucion Logistica", col=I("black"), fill=I("grey")) 
qplot(X, main="Transformacion Distribucion Logistica", col=I("black"), fill=I("grey"))


#Grafico#
par(mfrow=c(1,2))
hist((L), main="Logic Tradicional", freq=F, xlab="", yalb="", col="magenta1", bty="n")
hist((X), main="Logic Transformacion", freq=F, xlab="", yalb="", col="purple1", bty="n") 


