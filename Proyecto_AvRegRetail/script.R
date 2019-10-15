library(tidyverse)
library(R2jags)

getwd()
setwd("C:/Users/FORANEA110/Desktop/REGRESION_AVANZADA/AvRegRetail")

datos <- read.csv("data_set.csv")
vtas <- read.csv("sales_data_set.csv")
tiendas<- read.csv("stores.csv")
vtas <- vtas %>% group_by(Store,Date) %>% summarise(sales = sum(Weekly_Sales))
datos <- inner_join(datos, vtas, by=c("Store","Date"))
datos <- inner_join(datos, tiendas, by=c("Store"))
datos$Date <-as.Date(datos$Date,"%d/%m/%Y")
set.seed(15)
datos <- datos %>% filter(Store %in% sample(1:45,10))
datos[is.na(datos[,5]),5] <- 0
datos[is.na(datos[,6]),6] <- 0
datos[is.na(datos[,7]),7] <- 0
datos[is.na(datos[,8]),8] <- 0
datos[is.na(datos[,9]),9] <- 0
datos$sales <- datos$sales/max(datos$sales)
datos$IsHoliday <- as.numeric(datos$IsHoliday)
datos$Date <- rep(1:143,10)
datos<-datos %>% filter(Type=="A") # Filtramos Tipo A
datos$Type <- as.numeric(datos$Type)
datos[,c(3:11,15)] <- scale(datos[,c(3:11,15)]) %>% as.data.frame()

unique(datos$Store)

write.table(datos, "completo.txt", sep="\t",row.names=F)
write.csv(datos,"completo.csv")



################DATA SET PRINCIPAL################

unique(datos$Store)

datos <- datos %>% filter(Store == 7)
vtas <- vtas %>% filter(Store == 7)
vtas <- vtas %>% group_by(Store, Date) %>% summarise(sales = sum(Weekly_Sales))

datos <- inner_join(datos, vtas, by=c("Store", "Date"))
datos$IsHoliday <- as.numeric(datos$IsHoliday)
datos$Date <- datos$Date %>% as.Date(., format = "%d/%m/%Y")
rm(vtas)
dates <- datos$Date
datos <- datos%>% arrange(Date) %>% mutate(date = 1:nrow(datos)) %>% select(-Date)
datos <- datos %>% select(-Store)
datos[is.na(datos[,3]),3] <- 0
datos[is.na(datos[,4]),4] <- 0
datos[is.na(datos[,5]),5] <- 0
datos[is.na(datos[,6]),6] <- 0
datos[is.na(datos[,7]),7] <- 0
summary(datos)
plot(datos)
datos$sales <- datos$sales/max(datos$sales)
datos[,c(1:10,12)] <- scale(datos[,c(1:10,12)]) %>% as.data.frame()

write.csv(datos, file="daniel.csv")
n <- nrow(datos)
data<-list("n"=n,"y"=datos$sales,"x"=datos[,c(1:10,12)])
inits<-function(){list(intercept=0, beta=rep(0,11), yf=rep(1,n), alpha=1)}

#-Parámetros a monitorear-
# Normal 
#parameters<-c("beta", "tau", "yf", "mu")
# Gamma
parameters<-c("intercept","beta", "yf", "mu")

mod.sim<-jags(data,inits,parameters,model.file="Gamma.txt",
              n.iter=10000,n.chains=1,n.burnin=1000, n.thin = 1)

out.sum<-mod.sim$BUGSoutput$summary
out<-mod.sim$BUGSoutput$sims.list

out.dic<-mod.sim$BUGSoutput$DIC
print(paste0("DIC: ",out.dic))
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
print(paste0("PseudoR2: ", stats::cor(datos$sales, out.yf[,1])^2))


print("Beta:")
out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
z<-out$beta[,5]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
print("Y pred Hombres:")
out.sum.t<-out.sum[grep("yf",rownames(out.sum)),c(1,3,7)]
print(out.sum.t)


out.yf<-out.sum[grep("yf",rownames(out.sum)),]
or<-order(datos$date)
ymin<-min(datos$sales,out.yf[,c(1,3,7)])
ymax<-max(datos$sales,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(datos$date,datos$sales,ylim=c(ymin,ymax))


lines(datos$date[or],out.yf[or,1],lwd=2,col=2)
lines(datos$date[or],out.yf[or,3],lty=2,col=2)
lines(datos$date[or],out.yf[or,7],lty=2,col=2)


####################################################### SERIE DE TIEMPO ###########################

datos <- read.csv("data_set.csv")
vtas <- read.csv("sales_data_set.csv")
colnames(vtas)
colnames(datos)
library(dplyr)

tienda1 <- vtas %>% 
  select(Store,Dept,Date,Weekly_Sales) %>%
  group_by(Store,Dept) %>%
  filter(Store==1)

tienda1$Date <-as.Date(tienda1$Date,"%d/%m/%Y")

#revisamos solo el 2010
test1 <- tienda1 %>%
  mutate(day = format(Date, "%d"),month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  filter(year==2010) %>%
  summarise(total = sum(Weekly_Sales))

#Revisamos  2010 y 2011
test2 <- tienda1 %>%
  mutate(day = format(Date, "%d"),month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  filter(year %in% c(2010,2011)) %>%
  summarise(total = sum(Weekly_Sales)) %>%
  arrange(year) %>%
  mutate(ano_mes = paste(year, month, sep=""))

test2<-as.data.frame(test2)
test2<-test2 %>% mutate_at(c(4), as.numeric)
str(test2)
#Redondear numeros
test2<-test2 %>% mutate(total=round(test2$total))

library(ggplot2)

#Graficamos 2010 
ggplot(test1,aes(x=month,y=total,group = 1)) +
  #scale_x_continuous(breaks = seq(from = 2006, to = 2017, by = 1), lim = c(2006,2016)) +
  geom_line(col="red") +
  xlab("Fecha") +
  ylab("Ventas semanales")

#Graficamos 2010 y 2011
ggplot(test2,aes(x=ano_mes,y=total,group = 1)) +
  #scale_x_continuous(breaks = seq(from = 2006, to = 2017, by = 1), lim = c(2006,2016)) +
  geom_line(col="red") +
  xlab("Fecha") +
  ylab("Ventas semanales")

########### MODELO ##############

library(scales)
library(R2jags)
library(knitr)
#-Reading data-
test2<-read.table("sales_store1.txt", header=TRUE)
n<-nrow(test2)
data<-list("n"=n,"y"=test2$total,"x"=test2$ano_mes)
data<-list("n"=n,"y"=c(test2$total[1:(n-1)],rep(NA,1)),"x"=test2$ano_mes) #para calcular nueva entrada
str(test2)
#-Defining inits-
inits.c<-function(){list(beta=rep(1,n),tau.b=1,yf1=rep(1,n))} # para el 6c
inits.d<-function(){list(mu=rep(1,n),tau.b=1,yf1=rep(1,n))} #para el 6d
#-Selecting parameters to monitor-
parameters.c<-c("beta","tau.b","yf1","mu") #para el 6c
parameters.d<-c("tau.b","yf1","mu") #para el 6d
#write.csv(test2,file="test2.csv")

#JAGS
ej6c.sim<-jags(data,inits.c,parameters.c,model.file="Ej6c.txt",
               n.iter=50000,n.chains=1,n.burnin=5000,n.thin=1)
out<-ej6c.sim$BUGSoutput$sims.list
out.sum<-ej6c.sim$BUGSoutput$summary
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
ymin<-min(test2$total[1:(n-1)],out.yf[,c(1,3,7)])
ymax<-max(test2[,2],out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(test2,type="l",col="grey80",ylim=c(ymin,ymax))
lines(test2[,1],out.yf[,1],lwd=2,col=2)
lines(test2[,1],out.yf[,3],lty=2,col="green3")
lines(test2[,1],out.yf[,7],lty=2,col="green3")
lines(test2[,1],out.yf[,5],lwd=2,col=4)

#JAGS
ej6d.sim<-jags(data,inits.d,parameters.d,model.file="Ej6d.txt",
               n.iter=50000,n.chains=1,n.burnin=5000,n.thin=1)
out<-ej6d.sim$BUGSoutput$sims.list
out.sum<-ej6d.sim$BUGSoutput$summary
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
ymin<-min(test2$total[1:(n-1)],out.yf[,c(1,3,7)])
ymax<-max(test2[,2],out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(test2,type="l",col="grey80",ylim=c(ymin,ymax))
lines(test2[,1],out.yf[,1],lwd=2,col=2)
lines(test2[,1],out.yf[,3],lty=2,col="green3")
lines(test2[,1],out.yf[,7],lty=2,col="green3")


########################################## MODELO 8 ##########################################

plot(datos)
pairs(datos)
cor(datos)
colnames(datos)

prob <- function(x){
  out <- min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#-Defining data-
n<-nrow(datos)
data<-list("n"=n,"y"=datos$sales,"x1"=datos$Size,"x2"=datos$CPI,"x3"=datos$MarkDown1,"x4"=datos$MarkDown5,"x5"=datos$Fuel_Price,"x6"=datos$Temperature)
data<-list("n"=n,"y"=c(datos$sales[1:(n-4)],NA,NA,NA,NA),"x1"=datos$Size,"x2"=datos$CPI,"x3"=datos$MarkDown1,"x4"=datos$MarkDown5,"x5"=datos$Fuel_Price,"x6"=datos$Temperature)

#-Defining inits-
inits<-function(){list(alpha=0,beta=rep(0,6),tau=1,yf1=rep(1,n))} #Eje 8a
inits<-function(){list(alpha=rep(0,n),beta=matrix(0,nrow=6,ncol=n),tau=1,tau.a=1,tau.b=rep(1,6),yf1=rep(1,n))} #Eje 8b
inits<-function(){list(alpha=0,beta=matrix(0,nrow=6,ncol=n),tau=1,yf1=rep(1,n))} #ejercico c
inits<-function(){list(alpha=rep(0,n),beta=rep(0,6),tau=1,yf1=rep(1,n))} #ejercico d

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","tau","yf1")

#-Running code-
#JAGS
ej8a.sim<-jags(data,inits,parameters,model.file="Ej8a.txt",
               n.iter=10000,n.chains=1,n.burnin=1000,n.thin=1)
ej8b.sim<-jags(data,inits,parameters,model.file="Ej8b.txt",
               n.iter=10000,n.chains=1,n.burnin=1000,n.thin=1)
ej8c.sim<-jags(data,inits,parameters,model.file="Ej8c.txt",
               n.iter=10000,n.chains=1,n.burnin=1000,n.thin=1)
ej8d.sim<-jags(data,inits,parameters,model.file="Ej8d.txt",
               n.iter=10000,n.chains=1,n.burnin=1000,n.thin=1)
#JAGS
out<-ej8a.sim$BUGSoutput$sims.list
out<-ej8b.sim$BUGSoutput$sims.list
out<-ej8c.sim$BUGSoutput$sims.list
out<-ej8d.sim$BUGSoutput$sims.list

#JAGS
out.sum<-ej8a.sim$BUGSoutput$summary
out.sum<-ej8b.sim$BUGSoutput$summary
out.sum<-ej8c.sim$BUGSoutput$summary
out.sum<-ej8d.sim$BUGSoutput$summary

#Tabla resumen
out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
print(out.sum.t)

#DIC
#out.dic<-ej8c.sim$DIC
out.dic<-ej8a.sim$BUGSoutput$DIC
out.dic<-ej8b.sim$BUGSoutput$DIC
out.dic<-ej8c.sim$BUGSoutput$DIC
out.dic<-ej8d.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
y<-data$y
ymin<-min(y[1:(n-4)],out.yf[,c(1,3,7)])
ymax<-max(y[1:(n-4)],out.yf[,c(1,3,7)])

#x1 vs. y
x<-data$x1
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#x2 vs. y
x<-data$x2
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#x3 vs. y
x<-data$x3
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#x4 vs. y
x<-data$x4
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#x5 vs. y
x<-data$x5
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#x6 vs. y
x<-data$x6
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#t vs. y
x<-datos$Date
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)

#betas
out.beta<-out.sum[grep("beta",rownames(out.sum)),]
plot(out.beta[1:104,1],type="l")
abline(h=-1.0376912, col=2)
plot(out.beta[105:208,1],type="l")
plot(out.beta[209:312,1],type="l")

#alpha
out.alpha<-out.sum[grep("alpha",rownames(out.sum)),]
plot(out.alpha[,1],type="l")
