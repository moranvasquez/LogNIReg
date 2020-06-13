# This function computes the PDF of the 
# multivariate slash distribution.

dmvslash <- function(x,mu,sigma,nu){

    if(nu<250){
      integrand <- Vectorize(function(u){mnormt::dmnorm(x*sqrt(u),mu*sqrt(u),sigma)*stats::dbeta(u,nu,1)})
      dens.slash <- integrate(integrand,0,1)$value
    }
    else{
      integrand <- Vectorize(function(u){mnormt::dmnorm(x*sqrt(u),mu*sqrt(u),sigma)})
      set.seed(123)
      u <- rbeta(3000,nu,1)
      dens.slash <- mean(integrand(u))
    }
    
  return(dens.slash)  
}

if(FALSE){
  # Example 1
  x <- c(3.5,2.5,4.5)
  mu <- c(4,2.5,3)
  sigma <- matrix(c(3.0,0.3,-1.3,0.3,0.7,-1.2,-1.3,-1.2,5.1),nc=3)
  nu <- 3
  dmvslash(x,mu,sigma,nu)
  nu <- 10000000
  dmvslash(x,mu,sigma,nu)
  mnormt::dmnorm(x,mu,sigma)
  x <- 2
  mu <- 1.8
  sigma <- 2
  nu <- 3
  dmvslash(x,mu,sigma,nu)
  nu <- 10000000
  dmvslash(x,mu,sigma,nu)
  mnormt::dmnorm(x,mu,sigma)
}