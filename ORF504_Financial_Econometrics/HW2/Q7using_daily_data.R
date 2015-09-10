######Q7a#########
setwd("C:/Courses/ORF504Financial Econometrics/HW2")
Temp=read.csv("MerckDaily.csv",header=TRUE)
cp2<-Temp[,7]
L1=length(cp2)
cp1<-rep(0,L1)
for(i in 1:L1)
cp1[i]<-cp2[L1+1-i]

L=floor(L1/21)
cp<-rep(0,L)
for(i in 1:L)
  cp[i]=cp1[21*(i-1)+1]
row=sum(cp[2:L]*cp[1:L-1])/sum(cp[1:L-1]^2)
xmean2=sum(cp[2:L])/(L-1)
xmean1=sum(cp[1:L-1])/(L-1)
rowd=sum((cp[2:L]-xmean2)*(cp[1:L-1]-xmean1))/sum((cp[1:L-1]-xmean1)^2)
test=(row-1)*L
testd=(rowd-1)*L
test
testd
######Q7b#########
logprice<-log(cp)
logreturn<-rep(0,L)
for(i in 2:L)
  logreturn[i]=logprice[i]-logprice[i-1]
Box.test(logreturn^2,lag=5,type="Ljung-Box")
Box.test(logreturn^2,lag=10,type="Ljung-Box")
######Q7c#########
arima(logreturn^2,order=c(1,0,0))
arima(logreturn^2,order=c(1,0,1))
arima(logreturn^2,order=c(0,0,1))
arima(logreturn^2,order=c(2,0,0))
arima(logreturn^2,order=c(0,0,2))
arima(logreturn^2,order=c(0,0,0))
######Q7d#########
answer<-arima(logreturn,order=c(0,0,1))$residuals
Box.test(answer,lag=1,type="Ljung-Box")
Box.test(answer,lag=5,type="Ljung-Box")
Box.test(answer,lag=10,type="Ljung-Box")



