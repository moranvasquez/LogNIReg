# This function computes the PDF of the 
# multivariate log-normal distribution.

dmvlognormal <- function(y, mu, sigma, log = FALSE){

  if(length(y)>1){  
  if( all( y > 0 ) )
  {
    dens.lognormal <- mnormt::dmnorm(x=log(y),mean=log(mu),sigma)*prod(1/y)
    if(log)
      return(log(dens.lognormal))
    } 
    else{
    if(log)
      return(-Inf)
    
      return(0)
    }
    }
  else{
    if(y>0){
    dens.lognormal <- stats::dnorm(x=log(y),mean=log(mu),sd=sqrt(sigma))*(1/y)
    if(log)
      return(log(dens.lognormal))
    }
    else{
      if(log)
        return(-Inf)
      
      return(0)
  }
  }
  return(dens.lognormal)
}  
  

if(FALSE){
  # Multivariate
  y <- c(3.5,2.5,4.5)
  mu <- c(4,2.5,3)
  sigma <- matrix(c(3.0,0.3,-1.3,0.3,0.7,-1.2,-1.3,-1.2,5.1),nc=3)
  dmvlognormal(y,mu,sigma)
  mnormt::dmnorm(log(y),log(mu),sigma)*prod(1/y)
  dmvlognormal(y,mu,sigma,log=T)
  log(mnormt::dmnorm(log(y),log(mu),sigma)*prod(1/y))
  # Univariate
  y <- 3
  mu <- 3.5
  sigma <- 2
  dmvlognormal(y,mu,sigma)
  dnorm(log(y),log(mu),sqrt(sigma))*prod(1/y)
  dmvlognormal(y,mu,sigma,log=T)
  log(dnorm(log(y),log(mu),sqrt(sigma))*prod(1/y))
}
