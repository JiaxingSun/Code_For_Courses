setwd("C:/Courses/ORF504Financial Econometrics/HW3")
Temp=read.csv("Intel.csv",header=TRUE)
cp1<-Temp[,7]
cp<-rep(0,length(cp1))
for(i in 1:length(cp1))
{
  cp[i]=cp1[length(cp1)-i+1]
}
return<-rep(0,length(cp)-1)
for(i in 1:length(cp)-1)
{
  return[i]=log(cp[i+1])-log(cp[i])
}



fitmodel<-garchFit(formula=~arma(3,0)+garch(1,1),data=return,cond.dist="norm",trace=FALSE)
S<-fitmodel@fit$cvar
alpha=0.00820701
beta1=0.00692554
beta2=0.01586366
beta3=0.04175989
c=0.00039117
b=0.05012590
a=0.91909062
A=1-beta1-beta2-beta3
f<-c(A,alpha,alpha,alpha)/A^2
S1<-S[1:4,1:4]
B=1-b-a
g<-c(B,c,c)/(B^2*2*(c/(1-a-b))^0.5)
S2<-S[5:7,5:7]
alpha/A
c/B
t(f)%*%S1%*%f
t(g)%*%S2%*%g
sqrt(t(f)%*%S1%*%f)
sqrt(t(g)%*%S2%*%g)
#######(f)#########
res<-fitmodel@residuals
acf(res, main="ACF for residuals")
Box.test(res,lag=5,type="Ljung-Box")
Box.test(res,lag=10,type="Ljung-Box")
acf(res^2,main="ACF for squared residuals")
Box.test(res^2,lag=5,type="Ljung-Box")
Box.test(res^2,lag=10,type="Ljung-Box")

y<-quantile(res,prob=seq(0,1,0.005))
par(mfrow=c(3,2))
qqnorm(y)
qqline(y)
tdis3<-qt(seq(0,1,0.005),3)
tdis4<-qt(seq(0,1,0.005),4)
tdis5<-qt(seq(0,1,0.005),5)
tdis6<-qt(seq(0,1,0.005),6)
tdis7<-qt(seq(0,1,0.005),7)
qqplot(tdis3,y,xlab="Quantile-t3",ylab="Quantile of residuals", main="Residuals vs t-distribution df=3")
qqline(y)
qqplot(tdis4,y,xlab="Quantile-t4",ylab="Quantile of residuals", main="Residuals vs t-distribution df=4")
qqline(y)
qqplot(tdis5,y,xlab="Quantile-t5",ylab="Quantile of residuals", main="Residuals vs t-distribution df=5")
qqline(y)
qqplot(tdis6,y,xlab="Quantile-t6",ylab="Quantile of residuals", main="Residuals vs t-distribution df=6")
qqline(y)
qqplot(tdis7,y,xlab="Quantile-t7",ylab="Quantile of residuals", main="Residuals vs t-distribution df=7")
qqline(y)