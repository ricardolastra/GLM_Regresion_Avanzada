#install.packages("tidyverse")
library(tidyverse)
library(gtools)
wdir<-"c:/Users/FORANEA110/Desktop/REGRESION_AVANZADA"
setwd(wdir)

#Leemos datos
muerte<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/Mortalidad.csv",sep=",",header=TRUE)
n<-nrow(muerte)
colnames(muerte)

#Variables explicativas.
x_mean<-mutate(muerte, Mean = rowMeans(select(muerte,Edad.Inf,Edad.Sup), na.rm = TRUE))
x<-x_mean$Mean
yh<-logit(muerte$Muertes.H/muerte$Expuestos.H)
ym<-logit(muerte$Muertes.M/muerte$Expuestos.M)

#------------------------------------------------------INCISO A

#Graficas
plot(x)
abline(v=10,col="red")
plot(yh)
abline(v=10,col="red")
plot(ym)
abline(v=10,col="red")
plot(x,ym,type="l")
abline(v=65,col="red") #el punto medio entre 15 y 100 es 65....pintamos la linea en rojo para ver a partir de cuando la pendiente sube

#La variable "x" que indica que existe un incremento de edad constante, por ello se visualiza una diagonal con pendiente continua positiva.
#La relacion con cada "y" para hombres y mujeres es que las defunciones entre personas expuestas aumentan con la edad aparentemente de forma cosntante con pendiente positiva mas grande a partir de la edad media de una persona.


#------------------------------------------------------INCISO B
install.packages("R2jags")
library(R2jags)

prob <- function(x){
  out <- min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#Datos HOMBRES
data<-list("n"=n,"y"=yh,"x"=x)
#Datos MUJERES
data<-list("n"=n,"y"=ym,"x"=x)
#Inits
inits<-function(){list(alpha=0,beta=0,tau=1,yf1=rep(0,n))}
#parametros
parameters<-c("alpha","beta","tau","yf1")
#Modelo HOMBRES
h.sim<-jags(data,inits,parameters,model.file="modelo1.txt",
               n.iter=50000,n.chains=1,n.burnin=5000,n.thin=1)
#Modelo MUJERES
m.sim<-jags(data,inits,parameters,model.file="modelo1.txt",
            n.iter=50000,n.chains=1,n.burnin=5000,n.thin=1)
#DIC HOMBRES
out.dic<-h.sim$BUGSoutput$DIC
print(out.dic)

#DIC HMUJERES
out.dic<-m.sim$BUGSoutput$DIC
print(out.dic)

#JAGS
out.sum<-h.sim$BUGSoutput$summary

#Predicciones hombres
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
ymin<-min(yh,out.yf[,c(1,3,7)])
out.yf[,c(1,3,7)]
ymax<-max(yh,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(x,yh,ylim=c(ymin,ymax))

#Pseudo Rcuadrada hombres
plot(yh,out.yf[,1])
R2 <- (cor(yh,out.yf[,1]))^2
print(R2)

#JAGS
out.sum<-h.sim$BUGSoutput$summary

#Predicciones Mujeres
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
ymin<-min(ym,out.yf[,c(1,3,7)])
out.yf[,c(1,3,7)]
ymax<-max(ym,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(x,ym,ylim=c(ymin,ymax))

#Pseudo Rcuadrada Mujeres
plot(ym,out.yf[,1])
R2 <- (cor(ym,out.yf[,1]))^2
print(R2)


#--------------------------------------------------------INCISO C

zh<-muerte$Muertes.H/muerte$Expuestos.H
zm<-muerte$Muertes.M/muerte$Expuestos.M

#Diagrama de dispersion Hombres
plot(x,zh,type="l")
abline(v=65,col="red") #el punto medio entre 15 y 100 es 65....pintamos la linea en rojo para ver a partir de cuando la pendiente sube

#Diagrama de dispersion Mujeres
plot(x,zm,type="l")
abline(v=65,col="red") #el punto medio entre 15 y 100 es 65....pintamos la linea en rojo para ver a partir de cuando la pendiente sube

#--------------------------------------------------------INCISO D

yh<-muerte$Muertes.H
ym<-muerte$Muertes.M

nh<-muerte$Expuestos.H
nm<-muerte$Expuestos.M

#Datos HOMBRES
data<-list("n"=n,"y"=yh,"x"=x,"ne"=nh)
#Datos MUJERES
data<-list("n"=n,"y"=ym,"x"=x,"ne"=nm)
#Inits
inits<-function(){list(beta=rep(0,2),yf1=rep(1,n))}
#parametros
parameters<-c("beta","p", "yf1")
#Modelo HOMBRES
h.sim<-jags(data,inits,parameters,model.file="modelo2.txt",
            n.iter=50000,n.chains=1,n.burnin=5000,n.thin=1)

#Modelo MUJERES
m.sim<-jags(data,inits,parameters,model.file="modelo2.txt",
            n.iter=50000,n.chains=1,n.burnin=5000,n.thin=1)
#DIC HOMBRES
out.dic<-h.sim$BUGSoutput$DIC
print(out.dic)

#DIC HMUJERES
out.dic<-m.sim$BUGSoutput$DIC
print(out.dic)

#Predicciones hombres
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
ymin<-min(yh,out.yf[,c(1,3,7)])
out.yf[,c(1,3,7)]
ymax<-max(yh,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(x,yh,ylim=c(ymin,ymax))

#Pseudo Rcuadrada hombres
plot(yh,out.yf[,1])
R2 <- (cor(yh,out.yf[,1]))^2
print(R2)

out.sum<-h.sim$BUGSoutput$summary

#Predicciones Mujeres
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
or<-order(x)
ymin<-min(ym,out.yf[,c(1,3,7)])
out.yf[,c(1,3,7)]
ymax<-max(ym,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(x,ym,ylim=c(ymin,ymax))

#Pseudo Rcuadrada Mujeres
plot(ym,out.yf[,1])
R2 <- (cor(ym,out.yf[,1]))^2
print(R2)


ymin<-min(yh,out.yf[,c(1,3,7)])
ymax<-max(yh,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(yh,type="l",col="grey80",ylim=c(ymin,ymax))
#plot(ym,type="l",col="grey80",ylim=c(ymin,ymax))
lines(yh,out.yf[,1],lwd=2,col=2)
lines(yh,out.yf[,3],lty=2,col=1)
lines(yh,out.yf[,7],lty=2,col=1)
lines(yh,out.yf[,5],lwd=2,col=4)
