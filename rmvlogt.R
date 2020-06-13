# Random generation for the multivariate log-t distribution

rmvlogt <- function(n,mu,sigma,df){
  sam_t <- mnormt::rmt(n,mean = log(mu),S=sigma,df=df)
  return(exp(sam_t))
}

if(FALSE){
  n <- 500
  mu <- c(12,30)
  sigma <- matrix(c(0.2,0.005,0.005,0.1),2,2)
  df <- 4
  plot(rmvlogt(n, mu, sigma, df))
  ###
  n <- 100
  mu <- 10
  sigma <- 0.2
  df <- 5
  hist(rmvlogt(n, mu, sigma, df),freq=F)
  source("dmvlogt.R")
  graf_dlogt <- Vectorize(function(y){dmvlogt(y,mu,sigma,df)})
  y <- seq(0.1,100,0.1)
  curve(graf_dlogt,add=T)
}