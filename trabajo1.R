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
#Gr?fica comparativa:
x11()
plot(obs,main="Gr?fica frecuencia observada y esperada, distribucci?n Poisson(1.99)")
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
nsim2=14000
# M. tradicional #
L <- rlogis(nsim2, 0,2)

# Transformada Inversa #
a=0
b=2
U <- runif(nsim2,0,1)
X = a + b*log((U)/(1-U))

#Grafico#
x11()
par(mfrow=c(1,2))
hist((L), main="Logistica(0,2) en R", freq=F, col="grey", bty="n")
hist((X), main="Logistica(0,2) método de la transformada inversa", freq=F, col="grey", bty="n") 


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
T2 = table(cut(dat2, breaks=puntos))
plot(T2)







