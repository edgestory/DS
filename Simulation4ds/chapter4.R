#install.packages('random')
library(random)
library(tidyverse)
#install.packages("rgl", dependencies = TRUE)

x <- randomNumbers(n=10000, col=2 , min= 0, max=1e+06, check=T)/1000000

n <- length(x)
df <- data.frame(x1=x[1:(n-1)],x2=x[2:n])

ggplot(df, aes(x=x1, y=x2))+
  geom_point(size=0.1)+
  xlab('random numbers from random.org')+
  ylab('lag 1')


##란두 난수

result=c()
seed <-  123
randu <- function(n){
  for (i in 1:n){
    seed <- (65539*seed) %% (2^31)
      result[i] <- seed / 2^31
  }
  return(result)
}

plot3s <- function(func, n=10000){
  x <- func(n)
  require("rgl")
  x1 <- x[1:(n-2)]
  x2 <- x[2:(n-1)]
  x3 <- x[3:n]
  plot3d(x1, x2, x3, size=3)
  play3d(spin3d())
}

plot3s(randu)

randu(100)

plot3s(runif)

#메르센 트위스터
ok <- RNGkind()
op <- par(mfrow=c(1,2), mar=c(3,4,2,2))
set.seed(111)

hist(rnorm(1000), main="Mersenne Twister, Inversion", freq = F,
     xlim=c(-4,4),ylim=c(0,0.43), cex.main=0.7)

curve(dnorm(x), col=2, lty=1, lwd=2, add=T)
RNGkind("Super", "Box-Muller")
RNGkind()

hist(rnorm(100), main="super-Duper",freq = F,
     xlim = c(-4,4), ylim=c(0,0.43),cex.main=0.7)
curve(dnorm(x),col=2, lty=1, lwd=2, add=T)


u <- runif(100, 0 ,1)
ifelse(u <= 0.2,0,1)

library(MASS)

invExp <- function(n, lambda = 1){
  u <-runif(n)
  x <- -(1/lambda)*log(u)
  return(x)
}

lambda <- c(0.5, 1, 2, 5)
par(mar=c(3,3,3,0), mfrow = c(2,2))

for(l in lambda){
  sample <- invExp(10000,1)
  truehist(sample, nbins = 20, col="limegreen",
           main=paste("lambda=",l), xlab="")
  curve(dexp(x,1),from=1e-10, add = T)
}

sample(1:3, 10, replace = T, prob=c(3/7, 1/7, 3/7))

