# Quantile computation of the distribution 
# associated with the Mahalanobis distances
# of an multivariate slash distribution

qmslash <- function(q,p,nu){
  fdamaha <- function(s){
    return(pchisq(s, df=p) - (((2^nu)*gamma((p/2)+nu))/((s^nu)*gamma(p/2)))*pchisq(s, df=(p+2*nu))-q)
  }
  qtl <- uniroot(fdamaha,c(1e-10,100000000000))$root
  return(qtl)
}