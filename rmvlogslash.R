# Random generation for the multivariate log-slash distribution

rmvlogslash <- function(n,mu,sigma,nu){
  
  if(length(mu)>1){
    sam_slash <- matrix(NA,nr=n,nc=length(mu))
    for(i in 1:n){
      sam_slash[i,] <- log(mu) + (mnormt::rmnorm(1,mean = rep(0, length(mu)),sigma)/sqrt(stats::rbeta(1,nu,1)))
  }
  }
  else{
    sam_slash <- c()
    for(i in 1:n){
      sam_slash[i] <- log(mu) + (mnormt::rmnorm(1,mean = rep(0, length(mu)),sigma)/sqrt(stats::rbeta(1,nu,1)))
    }
    }
  return(exp(sam_slash))
}

if(FALSE){
  set.seed(123)
  n <- 100
  mu <- c(5,8)
  sigma <- matrix(c(1,0.3,0.3,0.7),2,2)
  nu <- 3
  m <- rmvlogslash(n, mu, sigma, nu)
  plot(m)
  ###
  n <- 100
  mu <- 10
  sigma <- 0.2
  df <- 5
  hist(rmvlogslash(n, mu, sigma, df),freq=F)
  source("dmvlogslash.R")
  graf_dlogslash <- Vectorize(function(y){dmvlogslash(y,mu,sigma,df)})
  y <- seq(0.1,100,0.1)
  curve(graf_dlogslash,add=T)
}