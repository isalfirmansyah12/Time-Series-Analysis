library(forecast)
library(FitAR)
library(lmtest)
library(tseries)
library(psych)
library(ggplot2)

setwd("D:/")
data<-read.csv("addw10_5.csv", sep=";")
data
summary(data)
#----------------
series=ts(data) ; series
ts.plot(data,ylab="Return",main="Plot Data Return Saham GWSA",col="turquoise",lwd=2)
legend("bottomright",c("Return"),cex=0.8,lty=5,text.font=2,col=c("turquoise"))

#Statitioner Varians
lambda=BoxCox.lambda(series) ; lambda
#Stationer Rata-rata
adf.test(series)
par(mfrow=c(1,2))
Acf(series, lag=20)
Pacf(series, lag=20)

#AR=PACF(p)|d=diff | MA=ACF (q)
model1=arima(series,order=c(1,0,0)) ; model1
coeftest(model1)
model2=arima(series,order=c(2,0,0)) ; model2
coeftest(model2)
model3=arima(series,order=c(0,0,1)) ; model3
coeftest(model3)
list(AIC(model1), AIC(model2),AIC(model3))

#Diagnostik Model
#RESIDUAL -----------
r1=residuals(model2)
#=====Uji Normalitas Data======
##Pengujian residual berdistribusi normal
#H0:residual berdistribusi normal
#H1:residual tidak berdistribusi normal
n1=length(r1)
mean1=mean(r1)
sd1=sd(r1)
res1=rnorm(n1,mean1,sd1)
cek.normalitas=ks.test(r1,res1) ; cek.normalitas

#=====Uji White Noise-Autokorelasi=====
#H0:residual white noise
#H1:residual tidak white noise
cek.WNA=Box.test(r1,lag=1,type=c("Ljung-Box")) ; cek.WNA

#=====Uji White Noise-Heteroskedastisitas=====
#H0:residual homogen (homoskedastisitas)
#H1:residual heterogen (heteroskedastisitas)
h1=r1^2
cek.heteros=Box.test(h1,type=c("Ljung-Box")) ; cek.heteros

install.packages("fGarch")
library(FinTS)
series.archTest <- ArchTest(series, lags = 1, demean = TRUE)
series.archTest

library(FinTS)
return.archTest <- ArchTest(series, lags = 1, demean = TRUE)
return.archTest

#Residuals:
res.arimamodel<-residuals(model2)
squared.res.arimamodel=res.arimamodel^2
par(mfcol=c(1,2))
plot(squared.res.arimamodel,main="Plot Squared Residuals Model ARIMA(2,0,0)", ylab="Squared Residuals", xlab="Time", col="turquoise",lwd=2)
ggAcf(squared.res.arimamodel, lag.max = 25)+ ggtitle("Plot ACF squared residual ARIMA (2,0,0)")
ggPacf(squared.res.arimamodel, lag.max = 25)+ ggtitle("Plot PACF squared residual ARIMA (2,0,0)")
Acf(squared.res.arimamodel, lag=20)
Pacf(squared.res.arimamodel, lag=20)

arch01=garch(res.arimamodel,order=c(0,1),trace=F)
loglik01=logLik(arch01)
summary(arch01)

arch02=garch(res.arimamodel,order=c(1,1),trace=F)
loglik02=logLik(arch02)
summary(arch02)


#untuk menampilkan p-value pada uji signifikansi parameter
coeftest(arch01)
coeftest(arch02)

#Pemilihan Model Terbaik menggunakan AIC
list(AIC(arch01),AIC(arch02))
summary(arch01)

#Generate 1-step forecast, 100-step forecast, and plot of forecast:
forecastmodelstep1=forecast(model2,1,level=95);forecastmodelstep1
forecastmodel=forecast(model2,12,level=95) ; forecastmodel
plot(forecastmodel, col="turquoise",ylab="Return", main="Plot Return GWSA dan Prediksi Return GWSA")

#Compute ht, conditional variance:
ht.arch01=arch01$fit[,1]^2 #use 1st column of fit
plot(ht.arch01,main='Conditional variances', col="turquoise", ylab="ht ARCH(5)")

#Generate plot of Return, 95% Upper and Lower limit
fitmodel=fitted.values(model2)
low=fitmodel-1.96*sqrt(ht.arch01)
high=fitmodel+1.96*sqrt(ht.arch01)
plot(series, type='l', ylab="Return", main="Plot Return, Low, and High Return GWSA",col="turquoise",lwd=2)
lines(low,col='red')
lines(high,col='blue')
legend("bottomright",c("Return", "Low", "High"),cex=0.6,lty=2,text.font=1,col=c("turquoise", "red", "blue"))