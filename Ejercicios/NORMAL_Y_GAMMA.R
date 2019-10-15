#NORMAL Y GAMMA EJERCICIO 7. UTILIDAD DE PROYECTO
#Dist Normal
x <- seq(-10,100,,100)
mu <- 39
sig <- 14.81
fx <- dnorm(x,mu,sig) 
fx
plot(x,fx,type="l",ylim = c(0,max(fx)+0.01))
abline(v=0,col=2,h=0)

#Funcion para convertir  parametros mu y sig en alpha y beta (limites 0,1  y 0,05^2 respectivamente)
#Opcion 1
estBetaParams <- function(mu, sig) {
  alpha <- ((1 - mu) / sig - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

#Opcion 2 que da el mismo resultado
estBetaParams <- function(mu, sig) {
  alpha <- -(mu * (sig + (mu ^ 2 - mu))) / sig
  beta <- ((sig + (mu ^ 2 - mu)) * (mu -1))/ sig
  return(params = list(alpha = alpha, beta = beta))
}

#Dist Gamma
alpha <- 49
fx2 <- pgamma(x, alpha, lower.tail = FALSE)
plot(x,fx2,type="l", ylab="y")
par(new=TRUE)
plot(x,fx,type="l", ylab="y")
abline(v=0,col=2,h=0)



