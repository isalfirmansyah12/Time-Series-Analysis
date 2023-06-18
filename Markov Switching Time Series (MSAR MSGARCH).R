library(tseries)
library(timeSeries)
library(timeDate)
library(forecast)
library(strucchange)
library(changepoint)
library(ggplot2)
options(install.lock = FALSE)
##Identifikasi Data##
## PLOT DATA ##
setwd("D:/Scripsweet")
dt<-read.csv("Kedatangan Domestik 90 2.csv",header=TRUE,sep=',')
DPS1<-dt[,2]

setwd("D:/Scripsweet")
dt<-read.csv("training stasioner.csv",header=TRUE,sep=',')
stasioner1<-dt[,2]
stasioner=ts(stasioner1)

setwd("D:/")
dt<-read.csv("growth90.csv",header=TRUE,sep=',')
DPS1<-dt[,3]

DPS=ts(DPS1)
ts.plot(DPS)

#Stationer Rata-rata
adf.test(DPS)

#ARMA
par(mfrow=c(2,1))
par(mfrow=c(1,1))
acf(DPS,lag.max=20)
pacf(DPS,lag.max=20)

fs.dataDPS <- Fstats(DPS ~ 1)
sctest(fs.dataDPS)

bp.dataDPS <- breakpoints(DPS ~ 1)
summary(bp.dataDPS)

plot(bp.dataDPS)
ts.plot(DPS)
lines(breakpoints(fs.dataDPS))
lines(fitted(bp.dataDPS)~1,col=4,lwd=2)

##Pengestimasian Model##
library(MSwM)
attach(dt)
modDPS=lm(DPS~1)
summary(modDPS)

#Pemodelan MSAR##

#DENPASAR(DPS)
##Model MS(2)-AR(0)
msDPS0=msmFit(modDPS,k=2,sw=c(TRUE,TRUE))
summary(msDPS0)

##Model MS(2)-AR(1)
msDPS1=msmFit(modDPS,k=2,sw=c(TRUE,FALSE,TRUE),p=1)
summary(msDPS1)

##Model MS(2)-AR(2)
msDPS2=msmFit(modDPS,k=2,sw=c(TRUE,FALSE,TRUE,FALSE),p=2)
summary(msDPS2)


residDPS0=msmResid(msDPS0)
residDPS1=msmResid(msDPS1)
residDPS2=msmResid(msDPS2)

## Uji residual nonautokorelasi ##
#DPS
autoDPS0=Box.test(residDPS0,type="Ljung-Box")
autoDPS0

autoDPS1=Box.test(residDPS1,type="Ljung-Box")
autoDPS1

autoDPS2=Box.test(residDPS2,type="Ljung-Box")
autoDPS2

## Uji normalitas residual ##
#USD
n0=length(residDPS0)
mean0=mean(residDPS0)
sd0=sd(residDPS0)
resn0=rnorm(n0,mean0,sd0)
ks.test(residDPS0,resn0)

n1=length(residDPS1)
mean1=mean(residDPS1)
sd1=sd(residDPS1)
resn1=rnorm(n1,mean1,sd1)
ks.test(residDPS1,resn1)

n2=length(residDPS2)
mean2=mean(residDPS2)
sd2=sd(residDPS2)
resn2=rnorm(n2,mean2,sd2)
ks.test(residDPS2,resn2)

## Uji Homogenitas varians residual ##
#DPS
homoDPS0=Box.test(residDPS0^2,type="Ljung-Box")
homoDPS0

homoDPS1=Box.test(residDPS1^2,type="Ljung-Box")
homoDPS1

homoDPS2=Box.test(residDPS2^2,type="Ljung-Box")
homoDPS2

## Loading Package ##
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
library(tseries)
library(aTSA)
require(timeDate)
require(fBasics)
library(MASS)
library(rlang)
library(ggplot2)
library(rmutil)
library(goftest)
library(zoo)
library(mnormt)
library(MSGARCH)
library(Rcpp)


## Pendugaan Parameter MSGARCH ##
#USD
ms2.garch.n.DPS1=CreateSpec(variance.spec = list(model = c("sGARCH")),distribution.spec = list(distribution = c("norm")),switch.spec = list(do.mix = FALSE, K = 2))
fit.DPS1 <- FitML(ms2.garch.n.DPS1, data = DPS,ctr = list(par(NULL)))
summary(fit.DPS1)

##Prediksi MSGARCH##
#USD
pred.its.DPS1 <- predict(object = fit.DPS1, nahead=15,do.return.draw=TRUE)
ramalan.DPS1<-pred.its.DPS1$draw[,1]
ramalan.DPS1