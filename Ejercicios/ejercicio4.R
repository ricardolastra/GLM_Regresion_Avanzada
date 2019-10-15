#By Ricardo Lastra
install.packages("R2jags")
library(R2jags)

wdir<-"c:/Users/FORANEA110/Desktop/REGRESION_AVANZADA"
setwd(wdir)

#--- Ejemplo 4 ---
#-Reading data-
salario<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/salarios.txt",header=TRUE)
n<-nrow(salario)
str(salario)
plot(salario$Y,salario$X1)
plot(salario$Y,salario$X2)
plot(salario$Y,salario$X3)
x<- data.frame(x1=salario$X1,x2=salario$X2,x3=salario$X3)

#-Defining data-
data<-list("n"=n,"y"=salario$Y,"x"=x)

#-Defining inits-
inits<-function(){list(beta=rep(0,3),tau=1,yf=rep(0,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","tau","yf")

#-Running code-
#OpenBUGS
#ej3.sim<-bugs(data,inits,parameters,model.file="Ej3.txt",
#              n.iter=10000,n.chains=1,n.burnin=1000)
#JAGS
ej4.sim<-jags(data,inits,parameters,model.file="C:/Users/FORANEA110/Desktop/REGRESION_AVANZADA/Ej4.txt",
              n.iter=10000,n.chains=1,n.burnin=1000,n.thin=1)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej4.sim)

#Cadena

#OpenBUGS
#out<-ej3.sim$sims.list

#JAGS
out<-ej4.sim$BUGSoutput$sims.list

z<-out$beta[,3]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

z<-out$beta
par(mfrow=c(1,1))
plot(z)

#Resumen (estimadores)
#OpenBUGS
#out.sum<-ej3.sim$summary

#JAGS
out.sum<-ej4.sim$BUGSoutput$summary

#Tabla resumen
out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
print(out.sum.t)

#DIC    (OJO CON ESTO)
out.dic<-ej4.sim$DIC
out.dic<-ej4.sim$BUGSoutput$DIC
print(out.dic)  ####### EL MODELO CON MENOR DIC AJUSTA MEJOR#######

#Predictions
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
or<-order(x)
ymin<-min(salario$Y,out.yf[,c(1,3,7)])
ymax<-max(salario$Y,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(x,salario$Y,ylim=c(ymin,ymax))
lines(x$x1[or],out.yf[or,1],lwd=2,col=2)
lines(calif$MO[or],out.yf[or,3],lty=2,col=2)
lines(calif$MO[or],out.yf[or,7],lty=2,col=2)

