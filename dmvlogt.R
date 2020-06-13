# This function computes the PDF of the 
# multivariate log-t distribution.

dmvlogt <- function(y, mu, sigma, df, log = FALSE){
  if( all( y > 0 ) ){
    dens.logt <- mnormt::dmt(x=log(y),mean=log(mu),S=sigma,df=df,log=F)*prod(1/y)
    if(log)
      return(log(dens.logt))
  }
  else{
    if(log)
      return(-Inf)
    
    return(0)
  }
  return(dens.logt)
}

if(FALSE){
  # Example 1
  y <- c(3.5,2.5,4.5)
  mu <- c(4,2.5,3)
  sigma <- matrix(c(3.0,0.3,-1.3,0.3,0.7,-1.2,-1.3,-1.2,5.1),nc=3)
  df <- 3
  dmvlogt(y,mu,sigma,df)
  df <- 2.8
  dmvlogt(y,mu,sigma,df)
  # Comparison between the univariate log-normal, log-t and 
  # log-slash densities
  source("dmvlognormal.R")
  source("dmvlogslash.R")
  y <- seq(0.1,8,0.1)
  mu <- 3
  sigma <- 0.1
  df <- 2.5
  graf_dlognormal <- Vectorize(function(y){dmvlognormal(y,mu,sigma)})
  graf_dlogt <- Vectorize(function(y){dmvlogt(y,mu,sigma,df)})
  graf_dlogslash <- Vectorize(function(y){dmvlogslash(y,mu,sigma,df)})
  plot(y,graf_dlognormal(y),type="l")
  lines(y,graf_dlogt(y),type="l",col="blue")
  lines(y,graf_dlogslash(y),type="l",col="red")
}
