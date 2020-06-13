# Random generation for the multivariate log-normal distribution

rmvlognormal <- function(n,mu,sigma){
  sam_normal <- mnormt::rmnorm(n,mean = log(mu),sigma)
  return(exp(sam_normal))
}

if(FALSE){
  n <- 500
  mu <- c(12,30)
  sigma <- matrix(c(1.2,0.2,0.2,0.5),2,2)
  plot(rmvlognormal(n, mu, sigma))
  ###
  n <- 100
  mu <- 10
  sigma <- 0.2
  hist(rmvlognormal(n, mu, sigma),freq=F)
  source("dmvlognormal.R")
  graf_dlognormal <- Vectorize(function(y){dmvlognormal(y,mu,sigma)})
  y <- seq(0.1,100,0.1)
  curve(graf_dlognormal,add=T)
}