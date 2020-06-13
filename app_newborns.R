# This code fits the multivariate log-normal, log-t 
# and log-slash linear regression models to newborn data
# Model fitting are made using package "heavy", version 0.38.19
# It can be installed using the following code lines:
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/heavy/heavy_0.38.19.tar.gz"
# install.packages(packageurl, repos=NULL,type="source")

data.newborns <- read.csv("newborns.csv",sep=",",header=TRUE,
                          fileEncoding = "UTF-8")
colnames(data.newborns)[colnames(data.newborns) == "PESO..Gramos."] <- "weight"
colnames(data.newborns)[colnames(data.newborns) == "TALLA..Centímetros."] <- "length"
colnames(data.newborns)[colnames(data.newborns) == "SEXO"] <- "gender"
colnames(data.newborns)[colnames(data.newborns) == "TIEMPO.DE.GESTACIÓN"] <- "gab"
colnames(data.newborns)[colnames(data.newborns) == "EDAD.PADRE"] <- "af"
colnames(data.newborns)[colnames(data.newborns) == "EDAD.MADRE"] <- "am"
data.newborns$gender <- as.factor(data.newborns$gender)
levels(data.newborns$gender)[1] <- "female"
levels(data.newborns$gender)[2] <- "male"
#data.newborns$gender[data.newborns$gender=="MASCULINO"] <- "male"
#data.newborns$gender[data.newborns$gender=="FEMENINO"] <- "female"
data.newborns <- data.newborns[data.newborns$af>0,]
data.newborns <- data.newborns[-which(is.na(data.newborns$af)),]
attach(data.newborns)
WL <- cbind(weight,length)
logWL <- log(WL)
n <- nrow(WL)
p <- ncol(WL)
data.female <- data.newborns[data.newborns$gender=="female",]
data.male <- data.newborns[data.newborns$gender=="male",]
weight.female <- data.female$weight
weight.male <- data.male$weight
length.female <- data.female$length
length.male <- data.male$length
gab.female <- data.female$gab
gab.male <- data.male$gab

##########################
## Descriptive analysis ##
##########################

# Bagplot: length vs. weight
library(aplpack)
par(mfrow=c(1,1),pty="s")
bagplot(WL,show.whiskers = F,axes=F,xlab="",ylab="",
        show.looppoints = F, show.bagpoints = F,
        show.baghull = TRUE,
        pch=1,col.baghull="gray", col.loophull="lightgray",
        cex=1.5)
box()
at.Y1 <- seq(from = 1000, to = 5000, by = 2000)
at.Y2 <- seq(from = 30, to = 54, by = 12)
axis(1,cex.axis=2.7, at=at.Y1, line = 0.1,tick = F)
axis(2,cex.axis=2.7, at=at.Y2, line = -0.8,tick = F)
title(xlab="Weight", line=3, cex.lab=2.5)
title(ylab="Length", line=2.4, cex.lab=2.5)

# Comparative boxplots: weight vs. gender
par(mfrow=c(1,1),pty="s")
boxplot(weight.female,weight.male,cex=1.5,
        axes=F)
box()
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = c(2, 1),labels = c("Male", "Female"))
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at.Y1)
title(xlab="Sex", line=3, cex.lab=2.5)
title(ylab="Weight", line=2.4, cex.lab=2.5)

# Comparative boxplots: length vs. gender
par(mfrow=c(1,1),pty="s")
boxplot(length.female,length.male,cex=1.5,axes=F)
box()
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = c(2, 1),labels = c("Male", "Female"))
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at.Y2)
title(xlab="Sex", line=3, cex.lab=2.5)
title(ylab="Length", line=2.4, cex.lab=2.5)

# Quartile curves: weight vs. gab

par(mfrow=c(1,1),pty="s")
boxp.Y1.gab <- boxplot(weight~gab,plot=F)
val.ejeX.Y1.gab <- as.numeric(as.vector(boxp.Y1.gab$names))
Y1.gab.25 <- boxp.Y1.gab$stats[2,]
Y1.gab.50 <- boxp.Y1.gab$stats[3,]
Y1.gab.75 <- boxp.Y1.gab$stats[4,]
lo.Y1.gab.25 <- loess(Y1.gab.25~val.ejeX.Y1.gab)
lo.Y1.gab.50 <- loess(Y1.gab.50~val.ejeX.Y1.gab)
lo.Y1.gab.75 <- loess(Y1.gab.75~val.ejeX.Y1.gab)
plot(val.ejeX.Y1.gab,predict(lo.Y1.gab.75),type="l",
     xaxt="n",yaxt="n",xlab="",ylab="",lty=5,lwd=2)
lines(val.ejeX.Y1.gab,predict(lo.Y1.gab.50),type="l",lty=1,lwd=2)
lines(val.ejeX.Y1.gab,predict(lo.Y1.gab.25),type="l",lty=4,lwd=2)
#legend("topleft",legend=c("Lower quartile", "Median", "Upper quartile"),
#       lty=c(4,1,5),y.intersp=1,cex=1.5,seg.len = 1.6,x.intersp=0.7,bty = "n",lwd=2)
at.weight.75.ejeY <- seq(from = 600, to = 3200, by = 1300)
at.gab <- seq(from = 24, to = 42, by = 9)
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = at.gab)
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at.weight.75.ejeY)
title(xlab="Gestational age at birth", line=3, cex.lab=2.5)
title(ylab="Weight", line=2.4, cex.lab=2.5)

# Quartile curves: length vs. gab
par(mfrow=c(1,1),pty="s")
boxp.Y2.gab <- boxplot(length~gab,plot=F)
val.ejeX.Y2.gab <- as.numeric(as.vector(boxp.Y2.gab$names))
Y2.gab.25 <- boxp.Y2.gab$stats[2,]
Y2.gab.50 <- boxp.Y2.gab$stats[3,]
Y2.gab.75 <- boxp.Y2.gab$stats[4,]
lo.Y2.gab.25 <- loess(Y2.gab.25~val.ejeX.Y2.gab)
lo.Y2.gab.50 <- loess(Y2.gab.50~val.ejeX.Y2.gab)
lo.Y2.gab.75 <- loess(Y2.gab.75~val.ejeX.Y2.gab)
plot(val.ejeX.Y2.gab,predict(lo.Y2.gab.75),type="l",
     xaxt="n",yaxt="n",xlab="",ylab="",lty=5,lwd=2)
lines(val.ejeX.Y2.gab,predict(lo.Y2.gab.50),type="l",lty=1,lwd=2)
lines(val.ejeX.Y2.gab,predict(lo.Y2.gab.25),type="l",lty=4,lwd=2)
#legend("topleft",legend=c("Lower quartile", "Median", "Upper quartile"),
#       lty=c(4,1,5),y.intersp=1,cex=1.5,seg.len = 1.6,x.intersp=0.7,bty = "n",lwd=2)
at.length.75.ejeY <- seq(from = 30, to = 50, by = 10)
at.gab <- seq(from = 24, to = 42, by = 9)
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = at.gab)
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at.length.75.ejeY)
title(xlab="Gestational age at birth", line=3, cex.lab=2.5)
title(ylab="Length", line=2.4, cex.lab=2.5)

# Quartile curves: weight vs. am
par(mfrow=c(1,1),pty="s")
boxp.Y1.am <- boxplot(weight~am,plot=F)
val.ejeX.Y1.am <- as.numeric(as.vector(boxp.Y1.am$names))
Y1.am.25 <- boxp.Y1.am$stats[2,]
Y1.am.50 <- boxp.Y1.am$stats[3,]
Y1.am.75 <- boxp.Y1.am$stats[4,]
lo.Y1.am.25 <- loess(Y1.am.25~val.ejeX.Y1.am)
lo.Y1.am.50 <- loess(Y1.am.50~val.ejeX.Y1.am)
lo.Y1.am.75 <- loess(Y1.am.75~val.ejeX.Y1.am)
plot(val.ejeX.Y1.am,predict(lo.Y1.am.75),type="l",
     xaxt="n",yaxt="n",xlab="",ylab="",lty=5,lwd=2,ylim=c(2250,4000))
lines(val.ejeX.Y1.am,predict(lo.Y1.am.50),type="l",lty=1,lwd=2)
lines(val.ejeX.Y1.am,predict(lo.Y1.am.25),type="l",lty=4,lwd=2)
#legend("topleft",legend=c("Lower quartile", "Median", "Upper quartile"),
#       lty=c(4,1,5),y.intersp=1,cex=1.5,seg.len = 1.6,x.intersp=0.7,bty = "n",lwd=2)
at1.weight.75.ejeY <- seq(from = 2300, to = 3800, by = 750)
at1.am <- seq(from = 14, to = 46, by = 16)
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = at1.am)
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at1.weight.75.ejeY)
title(xlab="Age of mother", line=3, cex.lab=2.5)
title(ylab="Weight", line=2.4, cex.lab=2.5)

# Quartile curves: length vs. am

par(mfrow=c(1,1),pty="s")
boxp.Y2.am <- boxplot(length~am,plot=F)
val.ejeX.Y2.am <- as.numeric(as.vector(boxp.Y2.am$names))
Y2.am.25 <- boxp.Y2.am$stats[2,]
Y2.am.50 <- boxp.Y2.am$stats[3,]
Y2.am.75 <- boxp.Y2.am$stats[4,]
lo.Y2.am.25 <- loess(Y2.am.25~val.ejeX.Y2.am)
lo.Y2.am.50 <- loess(Y2.am.50~val.ejeX.Y2.am)
lo.Y2.am.75 <- loess(Y2.am.75~val.ejeX.Y2.am)
plot(val.ejeX.Y2.am,predict(lo.Y2.am.75),type="l",
     xaxt="n",yaxt="n",xlab="",ylab="",lty=5,lwd=2,ylim=c(45,55))
lines(val.ejeX.Y2.am,predict(lo.Y2.am.50),type="l",lty=1,lwd=2)
lines(val.ejeX.Y2.am,predict(lo.Y2.am.25),type="l",lty=4,lwd=2)
#legend("topleft",legend=c("Lower quartile", "Median", "Upper quartile"),
#       lty=c(4,1,5),y.intersp=1,cex=1.5,seg.len = 1.6,x.intersp=0.7,bty = "n",lwd=2)
at1.length.75.ejeY <- seq(from = 45, to = 55, by = 5)
at1.am <- seq(from = 14, to = 46, by = 16)
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = at1.am)
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at1.length.75.ejeY)
title(xlab="Age of mother", line=3, cex.lab=2.5)
title(ylab="Length", line=2.4, cex.lab=2.5)

# Quartile curves: weight vs. af
par(mfrow=c(1,1),pty="s")
boxp.Y1.af <- boxplot(weight~af,plot=F)
val.ejeX.Y1.af <- as.numeric(as.vector(boxp.Y1.af$names))
Y1.af.25 <- boxp.Y1.af$stats[2,]
Y1.af.50 <- boxp.Y1.af$stats[3,]
Y1.af.75 <- boxp.Y1.af$stats[4,]
lo.Y1.af.25 <- loess(Y1.af.25~val.ejeX.Y1.af)
lo.Y1.af.50 <- loess(Y1.af.50~val.ejeX.Y1.af)
lo.Y1.af.75 <- loess(Y1.af.75~val.ejeX.Y1.af)
plot(val.ejeX.Y1.af,predict(lo.Y1.af.75),type="l",
     xaxt="n",yaxt="n",xlab="",ylab="",lty=5,lwd=2,ylim=c(2250,4000))
lines(val.ejeX.Y1.af,predict(lo.Y1.af.50),type="l",lty=1,lwd=2)
lines(val.ejeX.Y1.af,predict(lo.Y1.af.25),type="l",lty=4,lwd=2)
#legend("topleft",legend=c("Lower quartile", "Median", "Upper quartile"),
#       lty=c(4,1,5),y.intersp=1,cex=1.5,seg.len = 1.6,x.intersp=0.7,bty = "n",lwd=2)
at1.weight.75.ejeY <- seq(from = 2300, to = 3800, by = 750)
at1.af <- seq(from = 16, to = 70, by = 27)
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = at1.af)
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at1.weight.75.ejeY)
title(xlab="Age of father", line=3, cex.lab=2.5)
title(ylab="Weight", line=2.4, cex.lab=2.5)

# Quartile curves: length vs. af
par(mfrow=c(1,1),pty="s")
boxp.Y2.af <- boxplot(length~af,plot=F)
val.ejeX.Y2.af <- as.numeric(as.vector(boxp.Y2.af$names))
Y2.af.25 <- boxp.Y2.af$stats[2,]
Y2.af.50 <- boxp.Y2.af$stats[3,]
Y2.af.75 <- boxp.Y2.af$stats[4,]
lo.Y2.af.25 <- loess(Y2.af.25~val.ejeX.Y2.af)
lo.Y2.af.50 <- loess(Y2.af.50~val.ejeX.Y2.af)
lo.Y2.af.75 <- loess(Y2.af.75~val.ejeX.Y2.af)
plot(val.ejeX.Y2.af,predict(lo.Y2.af.75),type="l",
     xaxt="n",yaxt="n",xlab="",ylab="",lty=5,lwd=2,ylim=c(45,55))
lines(val.ejeX.Y2.af,predict(lo.Y2.af.50),type="l",lty=1,lwd=2)
lines(val.ejeX.Y2.af,predict(lo.Y2.af.25),type="l",lty=4,lwd=2)
#legend("topleft",legend=c("Lower quartile", "Median", "Upper quartile"),
#       lty=c(4,1,5),y.intersp=1,cex=1.5,seg.len = 1.6,x.intersp=0.7,bty = "n",lwd=2)
at1.length.75.ejeY <- seq(from = 45, to = 55, by = 5)
at1.af <- seq(from = 16, to = 70, by = 27)
axis(1,cex.axis=2.7, line = 0.1,tick = F, at = at1.af)
axis(2,cex.axis=2.7, line = -0.8,tick = F, at=at1.length.75.ejeY)
title(xlab="Age of father", line=3, cex.lab=2.5)
title(ylab="Length", line=2.4, cex.lab=2.5)

###############################################
## Fit of multivariate log-normal, log-t and ##
## log-slash linear regression models        ##
###############################################
set.seed(1977)
library(heavy)
################
## log-normal ##
################
fit.lognormal <- heavyLm(logWL ~ gender + gab + am + af, family = normal())

# AIC (log-normal)
num.param.lognormal <- prod(dim(fit.lognormal$coefficients)) + ncol(fit.lognormal$Sigma)*(ncol(fit.lognormal$Sigma)+1)/2
X <- model.matrix(~ gender + gab + am + af)
source("dmvlognormal.R")
l.lognormal <- c()
for(i in 1:n){
  l.lognormal[i] <- dmvlognormal(y=WL[i,], mu=exp(as.vector(t(fit.lognormal$coefficients)%*%X[i,])), sigma=fit.lognormal$Sigma, log = T)
}
AIC.lognormal <- 2*num.param.lognormal - 2*sum(l.lognormal)
AIC.lognormal

# QQ-plot of Mahalanobis distances with simulated envelope (log-normal)
Mahadist.lognormal <- sort(fit.lognormal$distances)
teoquant.lognormal <- qchisq((1:n/(n+1)), df=p)
e.lognormal <- matrix(0,n,100)
e1.lognormal <- numeric(n)
e2.lognormal <- numeric(n)
for(i in 1:100){
  est.sam.lognormal <- mnormt::rmnorm(n, rep(0,p), diag(p))
  e.lognormal[,i] <- sort(apply(est.sam.lognormal^2,1,sum))
}
for(i in 1:n){
  eo.lognormal <- sort(e.lognormal[i,])
  e1.lognormal[i] <- min(eo.lognormal)
  e2.lognormal[i] <- max(eo.lognormal)
}
par(mfrow=c(1,1),pty="s")
plot(teoquant.lognormal,Mahadist.lognormal,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,17.8),ylim=c(0,64.8),lty=1, main="",pch=19,cex=1)
at_1.ln <- seq(from = 1, to = 17, by = 8)
at_2.ln <- seq(from = 1, to = 65, by = 32)
axis(side = 1, at = at_1.ln,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at_2.ln,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Theoretical quantiles", line=3, cex.lab=2.5)
title(ylab="Observed quantiles", line=2.4, cex.lab=2.5)
lines(x=c(0,17.8),y=c(0,17.8),col=rgb(0.2,0.2,0.2),type="l",lty=2,lwd=3)
par(new=TRUE)
plot(teoquant.lognormal,sort(e1.lognormal),axes=F,xlab="",ylab="",type="l",xlim=c(0,17.8),ylim=c(0,64.8),lwd=3)
par(new=TRUE)
plot(teoquant.lognormal,sort(e2.lognormal),axes=F,xlab="",ylab="",type="l",xlim=c(0,17.8),ylim=c(0,64.8),lwd=3)  

###########
## log-t ##
###########
fit.logt <- heavyLm(logWL ~ gender + gab + am + af, family = Student())

# AIC (log-t)
num.param.logt <- prod(dim(fit.logt$coefficients)) + 1 + ncol(fit.logt$Sigma)*(ncol(fit.logt$Sigma)+1)/2
source("dmvlogt.R")
l.logt <- c()
for(i in 1:n){
  l.logt[i] <- dmvlogt(y=WL[i,], mu=exp(as.vector(t(fit.logt$coefficients)%*%X[i,])), sigma=fit.logt$Sigma, df=fit.logt$family$call$df, log = T)
}
AIC.logt <- 2*num.param.logt - 2*sum(l.logt)
AIC.logt
# QQ-plot of Mahalanobis distances with simulated envelope (log-t)
Mahadist.logt <- sort(fit.logt$distances)
teoquant.logt <- p*qf((1:n/(n+1)), df1=p, df2=fit.logt$family$call$df)
e.logt <- matrix(0,n,100)
e1.logt <- numeric(n)
e2.logt <- numeric(n)
for(i in 1:100){
  est.sam.logt <- mnormt::rmt(n, mean=rep(0,p), S=diag(p), df=fit.logt$family$call$df)
  e.logt[,i] <- sort(apply(est.sam.logt^2,1,sum))
}
for(i in 1:n){
  eo.logt <- sort(e.logt[i,])
  e1.logt[i] <- min(eo.logt)
  e2.logt[i] <- max(eo.logt)
}
par(mfrow=c(1,1),pty="s",mex=1.05)
plot(teoquant.logt,Mahadist.logt,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,54.4),ylim=c(0,97.6),lty=1, main="",pch=19,cex=1)
at_1.lt <- seq(from = 2, to = 54, by = 26)
at_2.lt <- seq(from = 2, to = 96, by = 47)
axis(side = 1, at = at_1.lt,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at_2.lt,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Theoretical quantiles", line=3, cex.lab=2.5)
title(ylab="Observed quantiles", line=2.4, cex.lab=2.5)
lines(x=c(0,54.4),y=c(0,54.4),col=rgb(0.2,0.2,0.2),type="l",lty=2,lwd=3)
par(new=TRUE)
plot(teoquant.logt,sort(e1.logt),axes=F,xlab="",ylab="",type="l",xlim=c(0,54.4),ylim=c(0,97.6),lwd=3)
par(new=TRUE)
plot(teoquant.logt,sort(e2.logt),axes=F,xlab="",ylab="",type="l",xlim=c(0,54.4),ylim=c(0,97.6),lwd=3)

###############
## log-slash ##
###############
fit.logslash <- heavyLm(logWL ~ gender + gab + am + af, family = slash())

# AIC (log-slash)
source("dmvlogslash.R")
num.param.logslash <- prod(dim(fit.logslash$coefficients)) + 1 + ncol(fit.logslash$Sigma)*(ncol(fit.logslash$Sigma)+1)/2
dmvlogslashWL <- Vectorize(function(i){dmvlogslash(WL[i,], mu=exp(as.vector(t(fit.logslash$coefficients)%*%X[i,])), sigma=fit.logslash$Sigma, nu=fit.logslash$family$call$df, log = T)})
i <- 1:nrow(WL)
l.logslash <- dmvlogslashWL(i)
AIC.logslash <- 2*num.param.logslash - 2*sum(l.logslash)
AIC.logslash
# QQ-plot of Mahalanobis distances with simulated envelope (log-slash)
Mahadist.logslash <- sort(fit.logslash$distances)
source("qmslash.R")
quant.mslash <- Vectorize(function(alpha){qmslash(q=alpha,p=p,nu=fit.logslash$family$call$df)})
teoquant.logslash <- quant.mslash((1:n)/(n+1))
e.logslash <- matrix(0,n,100)
e1.logslash <- numeric(n)
e2.logslash <- numeric(n)
for(i in 1:100){
  est.sam.logslash <-  rmslash(n, center=rep(0,p), Scatter=diag(p), df=fit.logslash$family$call$df)
  e.logslash[,i] <- sort(apply(est.sam.logslash^2,1,sum))
}
for(i in 1:n){
  eo.logslash <- sort(e.logslash[i,])
  e1.logslash[i] <- min(eo.logslash)
  e2.logslash[i] <- max(eo.logslash)
}
par(mfrow=c(1,1),pty="s",mex=1.05)
plot(teoquant.logslash,Mahadist.logslash,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,136.4),ylim=c(0,133),lty=1, main="",pch=19,cex=1)
at_1.ls <- seq(from = 2, to = 136, by = 67)
at_2.ls <- seq(from = 2, to = 136, by = 67)
axis(side = 1, at = at_1.ls,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at_2.ls,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Theoretical quantiles", line=3, cex.lab=2.5)
title(ylab="Observed quantiles", line=2.4, cex.lab=2.5)
lines(x=c(0,135),y=c(0,135),col=rgb(0.2,0.2,0.2),type="l",lty=2,lwd=3)
par(new=TRUE)
plot(teoquant.logslash,sort(e1.logslash),axes=F,xlab="",ylab="",type="l",xlim=c(0,136.4),ylim=c(0,133),lwd=3)
par(new=TRUE)
plot(teoquant.logslash,sort(e2.logslash),axes=F,xlab="",ylab="",type="l",xlim=c(0,136.4),ylim=c(0,133),lwd=3)

#############################################
## Estimates, SE, 95% confidence intervals ##
## and Wald test for for the model with    ##
## smaller AIC (log-slash)                 ##
#############################################
# Standard error
se.weight<-sqrt(diag(fit.logslash$acov)[1:5])
se.length<-sqrt(diag(fit.logslash$acov)[6:10])
# 95% confidence intervals
low.lim.weight <- fit.logslash$coefficients[,1] - qnorm(0.975)*sqrt(diag(fit.logslash$acov)[1:5])
up.lim.weight <- fit.logslash$coefficients[,1] + qnorm(0.975)*sqrt(diag(fit.logslash$acov)[1:5])
low.lim.length <- fit.logslash$coefficients[,2] - qnorm(0.975)*sqrt(diag(fit.logslash$acov)[6:10])
up.lim.length <- fit.logslash$coefficients[,2] + qnorm(0.975)*sqrt(diag(fit.logslash$acov)[6:10])
# Wald test
Wald.weight <- ((fit.logslash$coefficients[,1])^2)/(diag(fit.logslash$acov)[1:5])
pvalue.Wald.weight <- 1- pchisq(q=as.vector(Wald.weight), df=1)
Wald.length <- ((fit.logslash$coefficients[,2])^2)/(diag(fit.logslash$acov)[6:10])
pvalue.Wald.length <- 1- pchisq(q=as.vector(Wald.length), df=1)
estim.weight <- round(cbind(fit.logslash$coefficients[,0:1],se.weight,low.lim.weight,up.lim.weight,pvalue.Wald.weight),5)
colnames(estim.weight) <- c("Estimate", "SE", "Lower", "Upper", "p-value")
estim.length <- round(cbind(fit.logslash$coefficients[,c(0,2)],se.length,low.lim.length,up.lim.length,pvalue.Wald.length),5)
colnames(estim.length) <- c("Estimate", "SE", "Lower", "Upper", "p-value")
estim.weight
estim.length

#############################
## Final model (log-slash) ##
## with gender and gab     ##
#############################
final.model <- heavyLm(logWL ~ gender + gab, family = slash())
# AIC (final model)
num.param.final.model <- prod(dim(final.model$coefficients)) + 1 + ncol(final.model$Sigma)*(ncol(final.model$Sigma)+1)/2
Xfm <- model.matrix(~ gender + gab)
source("dmvlogslash.R")
dmv.final.model.WL <- Vectorize(function(i){dmvlogslash(WL[i,], mu=exp(as.vector(t(final.model$coefficients)%*%Xfm[i,])), sigma=final.model$Sigma, nu=final.model$family$call$df, log = T)})
i <- 1:nrow(WL)
l.final.model <- dmv.final.model.WL(i)
AIC.final.model <- 2*num.param.final.model - 2*sum(l.final.model)
AIC.final.model
# Estimates, SE, 95% confidence intervals
# and Wald test for for the final model
se.weight.fm<-sqrt(diag(final.model$acov)[1:3])
se.length.fm<-sqrt(diag(final.model$acov)[4:6])
# 95% confidence intervals
low.lim.weight.fm <- final.model$coefficients[,1] - qnorm(0.975)*sqrt(diag(final.model$acov)[1:3])
up.lim.weight.fm <- final.model$coefficients[,1] + qnorm(0.975)*sqrt(diag(final.model$acov)[1:3])
low.lim.length.fm <- final.model$coefficients[,2] - qnorm(0.975)*sqrt(diag(final.model$acov)[4:6])
up.lim.length.fm <- final.model$coefficients[,2] + qnorm(0.975)*sqrt(diag(final.model$acov)[4:6])
# Wald test
Wald.weight.fm <- ((final.model$coefficients[,1])^2)/(diag(final.model$acov)[1:3])
pvalue.Wald.weight.fm <- 1- pchisq(q=as.vector(Wald.weight.fm), df=1)
Wald.length.fm <- ((final.model$coefficients[,2])^2)/(diag(final.model$acov)[4:6])
pvalue.Wald.length.fm <- 1- pchisq(q=as.vector(Wald.length.fm), df=1)
estim.weight.fm <- round(cbind(final.model$coefficients[,0:1],se.weight.fm,low.lim.weight.fm,up.lim.weight.fm,pvalue.Wald.weight.fm),5)
colnames(estim.weight.fm) <- c("Estimate", "SE", "Lower", "Upper", "p-value")
estim.length.fm <- round(cbind(final.model$coefficients[,c(0,2)],se.length.fm,low.lim.length.fm,up.lim.length.fm,pvalue.Wald.length.fm),5)
colnames(estim.length.fm) <- c("Estimate", "SE", "Lower", "Upper", "p-value")
estim.weight.fm
estim.length.fm

# Dispersion matrix estimate
round(final.model$Sigma,5)
# Tail parameter estimate
round(final.model$family$call$df,5)

# AIC considering independence between weight
# and length, and fit univariate log-slash 
# linear regression model marginally
coef.weight <- final.model$coefficients[,1]
coef.length <- final.model$coefficients[,2]
fit.final.model.marg.weight <- heavyLm(log(weight) ~ gender + gab, family = slash())
num.param.final.model.marg.weight <- length(fit.final.model.marg.weight$coefficients) + 1 + length(fit.final.model.marg.weight$sigma2)
d.final.model.marg.weight <- Vectorize(function(i){dmvlogslash(weight[i], mu=exp(as.vector(t(fit.final.model.marg.weight$coefficients)%*%Xfm[i,])), sigma=fit.final.model.marg.weight$sigma2, nu=fit.final.model.marg.weight$family$call$df, log = T)})
i <- 1:length(weight)
l.fit.final.model.marg.weight <- d.final.model.marg.weight(i)
AIC.final.model.marg.weight <- 2*num.param.final.model.marg.weight - 2*sum(l.fit.final.model.marg.weight)
fit.final.model.marg.length <- heavyLm(log(length) ~ gender + gab, family = slash())
num.param.final.model.marg.length <- length(fit.final.model.marg.length$coefficients) + 1 + length(fit.final.model.marg.length$sigma2)
d.final.model.marg.length <- Vectorize(function(i){dmvlogslash(length[i], mu=exp(as.vector(t(fit.final.model.marg.length$coefficients)%*%Xfm[i,])), sigma=fit.final.model.marg.length$sigma2, nu=fit.final.model.marg.length$family$call$df, log = T)})
i <- 1:length(length)
l.fit.final.model.marg.length <- d.final.model.marg.length(i)
AIC.final.model.marg.length <- 2*num.param.final.model.marg.length - 2*sum(l.fit.final.model.marg.length)
# AIC
AIC.final.model.marg.indep <- AIC.final.model.marg.weight + AIC.final.model.marg.length
AIC.final.model.marg.indep

# QQ-plot of Mahalanobis distances with simulated envelope (final model)
Mahadist.final.model <- sort(final.model$distances)
quant.mslash <- Vectorize(function(alpha){qmslash(q=alpha,p=p,nu=final.model$family$call$df)})
teoquant.final.model <- quant.mslash((1:n)/(n+1))
e.final.model <- matrix(0,n,100)
e1.final.model <- numeric(n)
e2.final.model <- numeric(n)
for(i in 1:100){
  est.sam.final.model <-  rmslash(n, center=rep(0,p), Scatter=diag(p), df=final.model$family$call$df)
  e.final.model[,i] <- sort(apply(est.sam.final.model^2,1,sum))
}
for(i in 1:n){
  eo.final.model <- sort(e.final.model[i,])
  e1.final.model[i] <- min(eo.final.model)
  e2.final.model[i] <- max(eo.final.model)
}
par(mfrow=c(1,1),pty="s",mex=1.05)
plot(teoquant.final.model,Mahadist.final.model,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,132.8),ylim=c(0,132.8),lty=1, main="",pch=19,cex=1)
at_1.ls <- seq(from = 2, to = 132, by = 65)
at_2.ls <- seq(from = 2, to = 132, by = 65)
axis(side = 1, at = at_1.ls,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at_2.ls,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Theoretical quantiles", line=3, cex.lab=2.5)
title(ylab="Observed quantiles", line=2.4, cex.lab=2.5)
lines(x=c(0,132),y=c(0,132),col=rgb(0.2,0.2,0.2),type="l",lty=2,lwd=3)
par(new=TRUE)
plot(teoquant.final.model,sort(e1.final.model),axes=F,xlab="",ylab="",type="l",xlim=c(0,132.8),ylim=c(0,132.8),lwd=3)
par(new=TRUE)
plot(teoquant.final.model,sort(e2.final.model),axes=F,xlab="",ylab="",type="l",xlim=c(0,132.8),ylim=c(0,132.8),lwd=3)

# Fitted quantile curves according to gender
source("qslash.R")
alpha <- c(0.005,0.05,0.25,0.5,0.75,0.95,0.995)
q.stdslash.alpha <- sapply(alpha,Vectorize(function(alpha){qslash(p=alpha,mu=0,sigma2=1,nu=final.model$family$call$df)}))    

# Weight (female)
quant.weight.female.t1wf <- list()
quant.weight.female.t2wf <- list()
sqrt.sigma11 <- sqrt(final.model$Sigma[1,1])
sqrt.sigma22 <- sqrt(final.model$Sigma[2,2])
t1wf <- list(seq(22,39.4,0.05),seq(22,39.65,0.05),seq(22,39.55,0.05),
             seq(22,39.55,0.05),seq(22,39.55,0.05),seq(22,39.6,0.05),seq(22,39.5,0.05))
t2wf <- list(seq(40.7,44,0.05),seq(40.45,44,0.05),seq(40.55,44,0.05),
             seq(40.55,44,0.05),seq(40.5,44,0.05),seq(40.5,44,0.05),seq(40.6,44,0.05))
quant.weight.female.t1wf[[1]] <- exp(coef.weight[1] + coef.weight[3]*t1wf[[1]] + sqrt.sigma11*q.stdslash.alpha[1])
quant.weight.female.t2wf[[1]] <- exp(coef.weight[1] + coef.weight[3]*t2wf[[1]] + sqrt.sigma11*q.stdslash.alpha[1])
quant.weight.female.t1wf[[2]] <- exp(coef.weight[1] + coef.weight[3]*t1wf[[2]] + sqrt.sigma11*q.stdslash.alpha[2])
quant.weight.female.t2wf[[2]] <- exp(coef.weight[1] + coef.weight[3]*t2wf[[2]] + sqrt.sigma11*q.stdslash.alpha[2])
quant.weight.female.t1wf[[3]] <- exp(coef.weight[1] + coef.weight[3]*t1wf[[3]] + sqrt.sigma11*q.stdslash.alpha[3])
quant.weight.female.t2wf[[3]] <- exp(coef.weight[1] + coef.weight[3]*t2wf[[3]] + sqrt.sigma11*q.stdslash.alpha[3])
quant.weight.female.t1wf[[4]] <- exp(coef.weight[1] + coef.weight[3]*t1wf[[4]] + sqrt.sigma11*q.stdslash.alpha[4])
quant.weight.female.t2wf[[4]] <- exp(coef.weight[1] + coef.weight[3]*t2wf[[4]] + sqrt.sigma11*q.stdslash.alpha[4])
quant.weight.female.t1wf[[5]] <- exp(coef.weight[1] + coef.weight[3]*t1wf[[5]] + sqrt.sigma11*q.stdslash.alpha[5])
quant.weight.female.t2wf[[5]] <- exp(coef.weight[1] + coef.weight[3]*t2wf[[5]] + sqrt.sigma11*q.stdslash.alpha[5])
quant.weight.female.t1wf[[6]] <- exp(coef.weight[1] + coef.weight[3]*t1wf[[6]] + sqrt.sigma11*q.stdslash.alpha[6])
quant.weight.female.t2wf[[6]] <- exp(coef.weight[1] + coef.weight[3]*t2wf[[6]] + sqrt.sigma11*q.stdslash.alpha[6])
quant.weight.female.t1wf[[7]] <- exp(coef.weight[1] + coef.weight[3]*t1wf[[7]] + sqrt.sigma11*q.stdslash.alpha[7])
quant.weight.female.t2wf[[7]] <- exp(coef.weight[1] + coef.weight[3]*t2wf[[7]] + sqrt.sigma11*q.stdslash.alpha[7])
par(mfrow=c(1,1),pty="s")
plot(gab.female,weight.female,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(23,42),ylim=c(540,5090),lty=1, main="",cex=1.4,col="white")
at1wf.weight.female <- seq(from = 24, to = 42, by = 9)
at2wf.weight.female <- seq(from = 540, to = 5090, by = 2275)
axis(side = 1, at = at1wf.weight.female,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at2wf.weight.female,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Gestational age at birth", line=3, cex.lab=2.5)
title(ylab="Weight (female)", line=2.4, cex.lab=2.5)
for(i in 1:length(alpha)){
  lines(t1wf[[i]],quant.weight.female.t1wf[[i]],lwd=3)
  lines(t2wf[[i]],quant.weight.female.t2wf[[i]],lwd=3)
}
est.weight.female0.005 <- exp(coef.weight[1] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[1])
text(40,est.weight.female0.005+26, expression(paste(0.5,th)),srt=38,cex=1.2)
est.weight.female0.05 <- exp(coef.weight[1] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[2])
text(40,est.weight.female0.05+26, expression(paste(5,th)),srt=39,cex=1.2)
est.weight.female0.25 <- exp(coef.weight[1] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[3])
text(40,est.weight.female0.25+26, expression(paste(25,th)),srt=41,cex=1.2)
est.weight.female0.50 <- exp(coef.weight[1] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[4])
text(40,est.weight.female0.50+26, expression(paste(50,th)),srt=44,cex=1.2)
est.weight.female0.75 <- exp(coef.weight[1] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[5])
text(40,est.weight.female0.75+26, expression(paste(75,th)),srt=48,cex=1.2)
est.weight.female0.95 <- exp(coef.weight[1] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[6])
text(40,est.weight.female0.95+26, expression(paste(95,th)),srt=50,cex=1.2)
est.weight.female0.995 <- exp(coef.weight[1] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[7])
text(40,est.weight.female0.995+26, expression(paste(99.5,th)),srt=53,cex=1.2)

# Length (female)
quant.length.female.t1lf <- list()
quant.length.female.t2lf <- list()
t1lf <- list(seq(22,39.3,0.05),seq(22,39.51,0.05),seq(22,39.37,0.05),
             seq(22,39.37,0.05),seq(22,39.37,0.05),seq(22,39.37,0.05),seq(22,39.27,0.05))
t2lf <- list(seq(40.7,44,0.05),seq(40.45,44,0.05),seq(40.58,45,0.05),
             seq(40.58,46,0.05),seq(40.57,44,0.05),seq(40.55,44,0.05),seq(40.74,44,0.05))
quant.length.female.t1lf[[1]] <- exp(coef.length[1] + coef.length[3]*t1lf[[1]] + sqrt.sigma22*q.stdslash.alpha[1])
quant.length.female.t2lf[[1]] <- exp(coef.length[1] + coef.length[3]*t2lf[[1]] + sqrt.sigma22*q.stdslash.alpha[1])
quant.length.female.t1lf[[2]] <- exp(coef.length[1] + coef.length[3]*t1lf[[2]] + sqrt.sigma22*q.stdslash.alpha[2])
quant.length.female.t2lf[[2]] <- exp(coef.length[1] + coef.length[3]*t2lf[[2]] + sqrt.sigma22*q.stdslash.alpha[2])
quant.length.female.t1lf[[3]] <- exp(coef.length[1] + coef.length[3]*t1lf[[3]] + sqrt.sigma22*q.stdslash.alpha[3])
quant.length.female.t2lf[[3]] <- exp(coef.length[1] + coef.length[3]*t2lf[[3]] + sqrt.sigma22*q.stdslash.alpha[3])
quant.length.female.t1lf[[4]] <- exp(coef.length[1] + coef.length[3]*t1lf[[4]] + sqrt.sigma22*q.stdslash.alpha[4])
quant.length.female.t2lf[[4]] <- exp(coef.length[1] + coef.length[3]*t2lf[[4]] + sqrt.sigma22*q.stdslash.alpha[4])
quant.length.female.t1lf[[5]] <- exp(coef.length[1] + coef.length[3]*t1lf[[5]] + sqrt.sigma22*q.stdslash.alpha[5])
quant.length.female.t2lf[[5]] <- exp(coef.length[1] + coef.length[3]*t2lf[[5]] + sqrt.sigma22*q.stdslash.alpha[5])
quant.length.female.t1lf[[6]] <- exp(coef.length[1] + coef.length[3]*t1lf[[6]] + sqrt.sigma22*q.stdslash.alpha[6])
quant.length.female.t2lf[[6]] <- exp(coef.length[1] + coef.length[3]*t2lf[[6]] + sqrt.sigma22*q.stdslash.alpha[6])
quant.length.female.t1lf[[7]] <- exp(coef.length[1] + coef.length[3]*t1lf[[7]] + sqrt.sigma22*q.stdslash.alpha[7])
quant.length.female.t2lf[[7]] <- exp(coef.length[1] + coef.length[3]*t2lf[[7]] + sqrt.sigma22*q.stdslash.alpha[7])
par(mfrow=c(1,1),pty="s")
plot(gab.female,length.female,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(23,42),ylim=c(28,58),lty=1, main="",cex=1.4,col="white")
at1lf.length.female <- seq(from = 24, to = 42, by = 9)
at2lf.length.female <- seq(from = 28, to = 58, by = 15)
axis(side = 1, at = at1lf.length.female,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at2lf.length.female,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Gestational age at birth", line=3, cex.lab=2.5)
title(ylab="Length (female)", line=2.4, cex.lab=2.5)
for(i in 1:length(alpha)){
  lines(t1lf[[i]],quant.length.female.t1lf[[i]],lwd=3)
  lines(t2lf[[i]],quant.length.female.t2lf[[i]],lwd=3)
}
est.length.female0.005 <- exp(coef.length[1] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[1])
text(40,est.length.female0.005, expression(paste(0.5,th)),srt=34,cex=1.2)
est.length.female0.05 <- exp(coef.length[1] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[2])
text(40,est.length.female0.05, expression(paste(5,th)),srt=34,cex=1.2)
est.length.female0.25 <- exp(coef.length[1] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[3])
text(40,est.length.female0.25, expression(paste(25,th)),srt=35,cex=1.2)
est.length.female0.50 <- exp(coef.length[1] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[4])
text(40,est.length.female0.50, expression(paste(50,th)),srt=36,cex=1.2)
est.length.female0.75 <- exp(coef.length[1] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[5])
text(40,est.length.female0.75, expression(paste(75,th)),srt=34,cex=1.2)
est.length.female0.95 <- exp(coef.length[1] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[6])
text(40,est.length.female0.95, expression(paste(95,th)),srt=36,cex=1.2)
est.length.female0.995 <- exp(coef.length[1] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[7])
text(40,est.length.female0.995, expression(paste(99.5,th)),srt=38,cex=1.2)

# Weight (male)
quant.weight.male.t1wm <- list()
quant.weight.male.t2wm <- list()
sqrt.sigma11 <- sqrt(final.model$Sigma[1,1])
sqrt.sigma22 <- sqrt(final.model$Sigma[2,2])
t1wm <- list(seq(22,39.43,0.05),seq(22,39.64,0.05),seq(22,39.54,0.05),
             seq(22,39.56,0.05),seq(22,39.58,0.05),seq(22,39.61,0.05),seq(22,39.53,0.05))
t2wm <- list(seq(40.69,44,0.05),seq(40.42,44,0.05),seq(40.5,44,0.05),
             seq(40.49,44,0.05),seq(40.47,44,0.05),seq(40.43,44,0.05),seq(40.58,44,0.05))
quant.weight.male.t1wm[[1]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t1wm[[1]] + sqrt.sigma11*q.stdslash.alpha[1])
quant.weight.male.t2wm[[1]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t2wm[[1]] + sqrt.sigma11*q.stdslash.alpha[1])
quant.weight.male.t1wm[[2]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t1wm[[2]] + sqrt.sigma11*q.stdslash.alpha[2])
quant.weight.male.t2wm[[2]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t2wm[[2]] + sqrt.sigma11*q.stdslash.alpha[2])
quant.weight.male.t1wm[[3]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t1wm[[3]] + sqrt.sigma11*q.stdslash.alpha[3])
quant.weight.male.t2wm[[3]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t2wm[[3]] + sqrt.sigma11*q.stdslash.alpha[3])
quant.weight.male.t1wm[[4]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t1wm[[4]] + sqrt.sigma11*q.stdslash.alpha[4])
quant.weight.male.t2wm[[4]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t2wm[[4]] + sqrt.sigma11*q.stdslash.alpha[4])
quant.weight.male.t1wm[[5]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t1wm[[5]] + sqrt.sigma11*q.stdslash.alpha[5])
quant.weight.male.t2wm[[5]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t2wm[[5]] + sqrt.sigma11*q.stdslash.alpha[5])
quant.weight.male.t1wm[[6]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t1wm[[6]] + sqrt.sigma11*q.stdslash.alpha[6])
quant.weight.male.t2wm[[6]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t2wm[[6]] + sqrt.sigma11*q.stdslash.alpha[6])
quant.weight.male.t1wm[[7]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t1wm[[7]] + sqrt.sigma11*q.stdslash.alpha[7])
quant.weight.male.t2wm[[7]] <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*t2wm[[7]] + sqrt.sigma11*q.stdslash.alpha[7])
par(mfrow=c(1,1),pty="s")
plot(gab.male,weight.male,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(23,42),ylim=c(540,5090),lty=1, main="",cex=1.4,col="white")
at1wm.weight.male <- seq(from = 24, to = 42, by = 9)
at2wm.weight.male <- seq(from = 540, to = 5090, by = 2275)
axis(side = 1, at = at1wm.weight.male,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at2wm.weight.male,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Gestational age at birth", line=3, cex.lab=2.5)
title(ylab="Weight (male)", line=2.4, cex.lab=2.5)
for(i in 1:length(alpha)){
  lines(t1wm[[i]],quant.weight.male.t1wm[[i]],lwd=3)
  lines(t2wm[[i]],quant.weight.male.t2wm[[i]],lwd=3)
}
est.weight.male0.005 <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[1])
text(40,est.weight.male0.005+26, expression(paste(0.5,th)),srt=38,cex=1.2)
est.weight.male0.05 <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[2])
text(40,est.weight.male0.05+26, expression(paste(5,th)),srt=39,cex=1.2)
est.weight.male0.25 <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[3])
text(40,est.weight.male0.25+26, expression(paste(25,th)),srt=41,cex=1.2)
est.weight.male0.50 <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[4])
text(40,est.weight.male0.50+26, expression(paste(50,th)),srt=44,cex=1.2)
est.weight.male0.75 <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[5])
text(40,est.weight.male0.75+26, expression(paste(75,th)),srt=48,cex=1.2)
est.weight.male0.95 <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[6])
text(40,est.weight.male0.95+26, expression(paste(95,th)),srt=50,cex=1.2)
est.weight.male0.995 <- exp(coef.weight[1] + coef.weight[2] + coef.weight[3]*40 + sqrt.sigma11*q.stdslash.alpha[7])
text(40,est.weight.male0.995+26, expression(paste(99.5,th)),srt=53,cex=1.2)

# Length (male)
quant.length.male.t1lm <- list()
quant.length.male.t2lm <- list()
t1lm <- list(seq(22,39.3,0.05),seq(22,39.51,0.05),seq(22,39.38,0.05),
             seq(22,39.37,0.05),seq(22,39.37,0.05),seq(22,39.37,0.05),seq(22,39.27,0.05))
t2lm <- list(seq(40.7,44,0.05),seq(40.45,44,0.05),seq(40.58,45,0.05),
             seq(40.58,46,0.05),seq(40.57,44,0.05),seq(40.55,44,0.05),seq(40.74,44,0.05))
quant.length.male.t1lm[[1]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t1lm[[1]] + sqrt.sigma22*q.stdslash.alpha[1])
quant.length.male.t2lm[[1]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t2lm[[1]] + sqrt.sigma22*q.stdslash.alpha[1])
quant.length.male.t1lm[[2]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t1lm[[2]] + sqrt.sigma22*q.stdslash.alpha[2])
quant.length.male.t2lm[[2]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t2lm[[2]] + sqrt.sigma22*q.stdslash.alpha[2])
quant.length.male.t1lm[[3]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t1lm[[3]] + sqrt.sigma22*q.stdslash.alpha[3])
quant.length.male.t2lm[[3]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t2lm[[3]] + sqrt.sigma22*q.stdslash.alpha[3])
quant.length.male.t1lm[[4]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t1lm[[4]] + sqrt.sigma22*q.stdslash.alpha[4])
quant.length.male.t2lm[[4]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t2lm[[4]] + sqrt.sigma22*q.stdslash.alpha[4])
quant.length.male.t1lm[[5]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t1lm[[5]] + sqrt.sigma22*q.stdslash.alpha[5])
quant.length.male.t2lm[[5]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t2lm[[5]] + sqrt.sigma22*q.stdslash.alpha[5])
quant.length.male.t1lm[[6]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t1lm[[6]] + sqrt.sigma22*q.stdslash.alpha[6])
quant.length.male.t2lm[[6]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t2lm[[6]] + sqrt.sigma22*q.stdslash.alpha[6])
quant.length.male.t1lm[[7]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t1lm[[7]] + sqrt.sigma22*q.stdslash.alpha[7])
quant.length.male.t2lm[[7]] <- exp(coef.length[1] + coef.length[2] + coef.length[3]*t2lm[[7]] + sqrt.sigma22*q.stdslash.alpha[7])
par(mfrow=c(1,1),pty="s")
plot(gab.male,length.male,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(23,42),ylim=c(28,58),lty=1, main="",cex=1.4,col="white")
at1lm.length.male <- seq(from = 24, to = 42, by = 9)
at2lm.length.male <- seq(from = 28, to = 58, by = 15)
axis(side = 1, at = at1lm.length.male,cex.axis=2.7,line = 0.1,tick = F)
axis(side = 2, at = at2lm.length.male,cex.axis=2.7,line = -0.8,tick = F)
title(xlab="Gestational age at birth", line=3, cex.lab=2.5)
title(ylab="Length (male)", line=2.4, cex.lab=2.5)
for(i in 1:length(alpha)){
  lines(t1lm[[i]],quant.length.male.t1lm[[i]],lwd=3)
  lines(t2lm[[i]],quant.length.male.t2lm[[i]],lwd=3)
}
est.length.male0.005 <- exp(coef.length[1] + coef.length[2] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[1])
text(40,est.length.male0.005, expression(paste(0.5,th)),srt=34,cex=1.2)
est.length.male0.05 <- exp(coef.length[1] + coef.length[2] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[2])
text(40,est.length.male0.05, expression(paste(5,th)),srt=34,cex=1.2)
est.length.male0.25 <- exp(coef.length[1] + coef.length[2] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[3])
text(40,est.length.male0.25, expression(paste(25,th)),srt=35,cex=1.2)
est.length.male0.50 <- exp(coef.length[1] + coef.length[2] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[4])
text(40,est.length.male0.50, expression(paste(50,th)),srt=36,cex=1.2)
est.length.male0.75 <- exp(coef.length[1] + coef.length[2] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[5])
text(40,est.length.male0.75, expression(paste(75,th)),srt=34,cex=1.2)
est.length.male0.95 <- exp(coef.length[1] + coef.length[2] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[6])
text(40,est.length.male0.95, expression(paste(95,th)),srt=36,cex=1.2)
est.length.male0.995 <- exp(coef.length[1] + coef.length[2] + coef.length[3]*40 + sqrt.sigma22*q.stdslash.alpha[7])
text(40,est.length.male0.995, expression(paste(99.5,th)),srt=38,cex=1.2)

