# Quantile function for the univariate slash distribution

qslash <- function(p,mu,sigma2,nu){
    qtl.func <- function(w){
    set.seed(123)
    u <- rbeta(3000,nu,1)
    integrand <- Vectorize(function(u){stats::pnorm(q=w,mean=mu,sd=(sigma2/sqrt(u)))})
    return(mean(integrand(u))-p)
    }
    return(uniroot(qtl.func,c(-1e+16,1e+16))$root)
}