library (forecast) # --- BoxCox Arima auto . arima function is in forecast
library (tsoutliers) # --- tso function is in tsoutliers package
library (lmtest) # --- coeftest function is in lmtest package
library (tseries)

setwd("D:/Time Series/Intervensi")
dt<-read.csv("Domestik 2011.csv",header=TRUE,sep=',')
data1<-dt[,2]
data=ts(data1,start = c(2011,1),end = c(2021,12), frequency = 12)
dataarima=ts(data1,start = c(2011,1),end = c(2020,3), frequency = 12)
length(dataarima)
plot(dataarima)

datac=ts(data1,start = c(2011,1),end = c(2022,6), frequency = 12)
#OUTLIERS
DPS_outlier=tsoutliers :: tso(datac,maxit.iloop = 10)
DPS_outlier$outliers
plot(DPS_outlier)

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
r2=residuals(model15)
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

#OUTLIERS
DPS_outlier=tsoutliers :: tso(data,maxit.iloop = 10)
DPS_outlier$outliers
plot(DPS_outlier)

n <- length(data)
mo_ls1<- outliers("LS",112)
ls1<- outliers.effects(mo_tc1, n)
mo_tc1<- outliers("TC",127)
tc1<- outliers.effects(mo_ls1, n)
xreg.outlier=cbind(ls1,tc1)
intervensi=arima(data,order=c(0,1,1),seasonal = list(order=c (1,0,0), period=12),xreg=xreg.outlier)
intervensi
coeftest(intervensi)

step1= filter (1*(seq(data)>=112), filter =0,method = "rec", sides=1)*(-280002.42)
step2= filter (1*(seq(data)>=127), filter =0,method = "rec", sides=1)*(-208981.99)
pulse1= filter (1*(seq(data)==112), filter =0,method = "rec", sides=1)*(-280002.42)
pulse2= filter (1*(seq(data)==127), filter =0,method = "rec", sides=1)*(-208981.99)
intervensi1<-Arima(data, order = c(0,1,1),seasonal = list(order=c (1,0,0), period=12),include.drift = FALSE, xreg = step1+step2)
intervensi2<-Arima(data, order = c(0,1,1),seasonal = list(order=c (1,0,0), period=12),include.drift = FALSE, xreg = pulse1+pulse2)
intervensi3<-Arima(data, order = c(0,1,1),seasonal = list(order=c (1,0,0), period=12),include.drift = FALSE, xreg = step1+pulse2)
intervensi4<-Arima(data, order = c(0,1,1),seasonal = list(order=c (1,0,0), period=12),include.drift = FALSE, xreg = pulse1+step2)
accuracy(intervensi1)
accuracy(intervensi2)
accuracy(intervensi3)
accuracy(intervensi4)

xreg.rob2= forecast(auto.arima(step1+step2),h=10)$mean
forecast (intervensi1, xreg = xreg.rob2)
accuracy(intervensi1)

##Uji Diagnostik Model Intervensi
res.d=residuals(intervensi3)
rest_int=res.d

# Uji Non Auto Korelasi Residual Model Intervensi 
res_int=residuals(intervensi3) 
Box.test(res_int, lag=128, type = "Ljung-Box") 

# Uji Homoskedastisitas Residual Model Intervensi
Box.test(res_int^2,lag=128,type="Ljung-Box")

# Uji Normalitas Residual Model Intervensi
n_int=length(res_int)
mean_int=mean(res_int)
sd_int=sd(res_int) 

resn_int=rnorm(n_int,mean_int,sd_int)
ks.test(res_int,resn_int) 

plot(forecast(intervensi3,xreg = xreg.rob2))

#Akurasi Model Intervensi dalam SARIMA
Rsquare2=cor(fitted(intervensi1),data)
Rsquare2^2
Radj2=1-((1-Rsquare2^2)*(111/109))
Radj2

accuracy(intervensi2)

m1=forecast(model14,h=30)
m1
er1=data[124:150]-m1

MAPE.1=mean(abs(er1/data[124:150]))
MAPE.1

x3=fitted(intervensi1)
e3=data-x3
MAPE3<-sum(abs(e3/data))/150*100
MAPE3

x3=fitted(model14)
e3=data-x3
MAPE3<-sum(abs(e3/dataarima))/150*100
MAPE3
