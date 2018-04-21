#-----------------------------------#
# Kevin Steven Garcia Chica
# Cesar A. Saavedra 
# Simulacion Estadistica Trabajo 1
#-----------------------------------#
suppressMessages(library(ggplot2))

#-----------------------------------#
# Distribucion Poisson:
#-----------------------------------#
nsim=10000
l=1.99 #Parametro del articulo, Media de terremotos por a??o.

# Poisson con R #
RP <- rpois(nsim,l)

# Poisson con metodo propio #
N=nsim
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

#Pruebas de bondad de ajuste:
Z<-table(X)
obs<-Z/length(X)  #Observado
prob<-dpois(seq(0,9),1.99) #Esperado
#Gráfica comparativa:
x11()
plot(obs,main="Gráfica frecuencia observada y esperada, distribucción Poisson(1.99)")
points(seq(0,9),prob)

#Prueba de bondad de ajuste chi cuadrado:
est<-sum((obs-prob)^2/prob)  #Estadistico de prueba
valorp<-1-pchisq(est,9)
qchisq(0.95,8)   #Valor critico
pchisq(est,9,lower.tail = F) #Otra forma de sacar la probabilidad mayor que.

#Prueba de R:

#Grafico#
x11()
par(mfrow=c(1,2))
barplot(table(RP), main="Poisson(1.99) en R", xlab="Magnitud", ylab="")
barplot(table(X), main="Poisson(1.99) con el metodo propio", xlab="Magnitud", ylab="")

qplot(RP,main="Poisson(1.99) en R, tasa media terremotos", col=I("black"), fill=I("grey"))
qplot(X,main="Poisson(1.99) con el metodo propio, tasa media terremotos", col=I("black"), fill=I("grey"))

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


#Generacion variable de respuesta Y:
Y<-c(rep(0,7000),rep(1,7000)) # 0:Clientes malos, 1:Clientes buenos

