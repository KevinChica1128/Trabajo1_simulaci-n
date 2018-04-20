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
#Poisson con R:
Z<-rpois(1000,l)
t<-table(Z)

#Poisson con método propio:
l=5
N=1000
X<-c()
for (j in 1:N) {
  U<-runif(1)
  i=0 ; F=P=exp(-l)
  while (U>=F) {
    P=(l/(i+1))*P
    F=F+P
    i=i+1
  }
  X[j]=i
}
q<-table(X)

#Gráfico:
x11()
par(mfrow=c(1,2))
plot(t,type="h",main="Poisson(5) en R")
plot(q,type="h",main="Poisson(5) con el método propio")




#-----------------------------------#
#Distribucion Logistic:
#-----------------------------------#
# M. tradicional #
L <- rlogis(nsim, 3,4)

# Transformada Inversa #
a=3
b=4
U <- runif(nsim,0,1)
X = a + b*log((U)/(1-U))

datos <- data.frame(L,X)

#Grafico#
qplot(L, main="Distribucion Logistica", col=I("black"), fill=I("grey")) 
qplot(X, main="Transformacion Distribucion Logistica", col=I("black"), fill=I("grey"))


#Grafico#
x11()
par(mfrow=c(1,2))
hist((L), main="Logic Tradicional", freq=F, xlab="", yalb="", col="magenta1", bty="n")
hist((X), main="Logic Transformacion", freq=F, xlab="", yalb="", col="purple1", bty="n") 


#Generación variable de respuesta Y:
Y<-c(rep(0,7000),rep(1,7000)) #0:Clientes malos 1:Clientes buenos

