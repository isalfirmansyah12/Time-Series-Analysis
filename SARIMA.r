library (forecast) # -- BoxCox Arima auto . arima function is in forecast
library (tsoutliers) # -- tso function is in tsoutliers package
library (lmtest) # -- coeftest function is in lmtest package
library (tseries)

setwd("D:/Time Series/ARIMA")
dt<-read.csv("Domestik 2011.csv",header=TRUE,sep=',')
data1<-dt[,2]
data=ts(data1,start = c(2011,1),end = c(2021,12), frequency = 12)
dataarima=ts(data1,start = c(2011,1),end = c(2020,3), frequency = 12)
length(dataarima)
plot(dataarima)

#Uji Stasioneritas dalam Varians
lambda=BoxCox.lambda(dataarima) ; lambda
tf1=1/dataarima^lambda
lambda1=BoxCox.lambda(tf1)
lambda1
tf2=tf1^lambda1
lambda2=BoxCox.lambda(tf2)
lambda2

#Uji Stasoneritas dalam Rata-rata
adf.test(dataarima)
adf.test(dataarima,k=12)
stasioner=diff(dataarima, differences = 1)
adf.test(stasioner)
adf.test(stasioner,k=12)

##PLOT ACF PACF
#ACF PACF Jumlah Penumpang
par(mfrow=c(2,1))
acf(stasioner, lag=50) #lag 1
pacf(stasioner, lag=50) #lag 1

#Model Dugaan SARIMA
model1 = arima(dataarima, order = c(1,1,0), seasonal = list(order=c (1,0,0), period=12))
model2 = arima(dataarima, order = c(1,1,0), seasonal = list(order=c (1,0,1), period=12))
model3 = arima(dataarima, order = c(1,1,0), seasonal = list(order=c (1,0,2), period=12))
model4 = arima(dataarima, order = c(1,1,0), seasonal = list(order=c (0,0,1), period=12))
model5 = arima(dataarima, order = c(1,1,1), seasonal = list(order=c (0,0,2), period=12))
model6 = arima(dataarima, order = c(1,1,1), seasonal = list(order=c (1,0,0), period=12))
model7 = arima(dataarima, order = c(1,1,1), seasonal = list(order=c (1,0,1), period=12))
model8 = arima(dataarima, order = c(1,1,1), seasonal = list(order=c (1,0,2), period=12))
model9 = arima(dataarima, order = c(1,1,1), seasonal = list(order=c (0,0,1), period=12))
model10 = arima(dataarima, order = c(1,1,0), seasonal = list(order=c (0,0,2), period=12))
model11 = arima(dataarima, order = c(0,1,1), seasonal = list(order=c (1,0,0), period=12))
model12 = arima(dataarima, order = c(0,1,1), seasonal = list(order=c (1,0,1), period=12))
model13 = arima(dataarima, order = c(0,1,1), seasonal = list(order=c (1,0,2), period=12))
model14 = arima(dataarima, order = c(0,1,1), seasonal = list(order=c (0,0,1), period=12))
model15 = arima(dataarima, order = c(0,1,1), seasonal = list(order=c (0,0,2), period=12))

#Pengujian Parameter Model SARIMA Dugaan
coeftest(model1)
coeftest(model2)
coeftest(model3)
coeftest(model4)
coeftest(model5)
coeftest(model6)
coeftest(model7)
coeftest(model8)
coeftest(model9)
coeftest(model10)
coeftest(model11)
coeftest(model12)
coeftest(model13)
coeftest(model14)
coeftest(model15)

#Pemilihan Model Terbaik menggunakan AIC
list(AIC(model1), AIC(model4),AIC(model5),AIC(model6),AIC(model9),AIC(model10),AIC(model11),AIC(model14),AIC(model15))

#Diagnostik Model
#RESIDUAL -----------
#=====Uji Normalitas Data======
##Pengujian residual berdistribusi normal
#H0:residual berdistribusi normal
#H1:residual tidak berdistribusi normal
r2=residuals(model14)
n2=length(r2)
mean2=mean(r2)
sd2=sd(r2)
res2=rnorm(n2,mean2,sd2)
cek.normalitas=ks.test(r2,res2) ; cek.normalitas

#=====Uji White Noise-Autokorelasi=====
#H0:residual white noise
#H1:residual tidak white noise
r2=residuals(model15)
cek.WNA=Box.test(r2,lag=108,type=c("Ljung-Box")) ; cek.WNA

#=====Uji White Noise-Heteroskedastisitas=====
#H0:residual homogen (homoskedastisitas)
#H1:residual heterogen (heteroskedastisitas)
r2=residuals(model15)
h2=r2^2
cek.heteros=Box.test(h2,lag=108,type=c("Ljung-Box")) ; cek.heteros

Prediksi=forecast(model14,h=30)
