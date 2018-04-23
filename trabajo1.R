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
prob<-round(dpois(seq(0,9),1.99),4) #Esperado
#Gr?fica comparativa:
x11()
plot(obs,main="Grafica frecuencia observada y esperada, distribuccion Poisson(1.99)", ylab="Frecuencia esperada")
points(seq(0,9),prob, col="blue", pch=16)

#Prueba de bondad de ajuste chi cuadrado:
est<-sum((obs-prob)^2/prob)  #Estadistico de prueba
valorp<-1-pchisq(est,9)
qchisq(0.95,8)   #Valor critico
pchisq(est,9,lower.tail = F) #Otra forma de sacar la probabilidad mayor que.

#Grafico#
plot(obs, type="h", lwd=15, col="black", main="Grafica frecuencia observada y esperada", Ylab="Valor Prueba de bondad de ajuste")
points(seq(0,9),prob, lwd=2, col="green", type="h")
legend("topright",legend=c("Observado","Esperado"), 
       pch=c(19,19),col=c("green","black"), lty=1,2, bty="n", cex=1.5)

#Prueba de R:

#Grafico#
plot(table(RP), main="Poisson para Lambda = 1.99 ", xlab="Frecuencia de terremotos", ylab="Simulaciones")
points(seq(0,9), table(X), pch=16, col="red")
legend("topright",legend=c("Metodo Tradicional R","Metodo Transformada Inversa"), 
       pch=c(16, 16),col=c("black","red"), bty="n", cex=1.2)
#plot(table(X), main="Poisson(1.99) con el metodo propio", xlab="Magnitud", ylab="")


#-----------------------------------#
#Distribucion Logistic:
#-----------------------------------#
nsim2=14000
# M. tradicional #
L <- rlogis(nsim2, 0,2)

# Transformada Inversa #
a=0
b=2
U <- runif(nsim2,0,1)
X = a + b*log((U)/(1-U))
l=seq(-20,20, by=0.001)

#Grafico#
x11()
hist((X), main="Distribucion Logistica de parametro (0,2)", freq=F, col="grey", bty="n")
lines(l, dlogis(l,0,2), lwd=2, col="red")
legend("topleft",legend=c("Metodo Transformada Inversa","Metodo Tradicional R"), 
       pch=c("-,-"),col=c("black","red"), lty=1,2, bty="n")

#Generacion variable de respuesta Y:
Y<-c(rep(0,7000),rep(1,7000)) # 0:Clientes malos, 1:Clientes buenos

# Tabla de frecuencias
dat1 = L
n.cl=8
puntos = min(dat1)+(0:n.cl)*(max(dat1)-min(dat1))/n.cl
T1 = table(cut(dat1, breaks=puntos))
plot(T1)

dat2 = X
n.cl=8
puntos = min(dat)+(0:n.cl)*(max(dat2)-min(dat2))/n.cl
T2 = table(cut(dat2, breaks=puntos,right = T))
plot(T2)

#Prueba de bondad de ajuste kolmogorov para la logistic:
ks.test(X,'plogis',0,2)





