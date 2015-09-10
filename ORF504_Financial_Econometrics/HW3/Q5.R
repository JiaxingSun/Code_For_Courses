setwd("C:/Courses/ORF504Financial Econometrics/HW3")
Temp=read.csv("Ford.csv",header=TRUE)
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
##################5(a)#######################
fit<-garchFit(formula=~arma(1,1)+garch(1,1),data=return,cond.dist="std",algorithm="nlminb",include.mean=TRUE)
summary(fit)
##################5(b)#######################
fit2<-garchFit(formula=~arma(1,1)+garch(1,1),data=return,cond.dist="norm",algorithm="nlminb",include.mean=TRUE)
summary(fit2)
##################5(c)#######################
mean(return)
var(return)
##################5(d)#######################
par(mfrow=c(2,2))
rt<-fit@residuals
rn<-fit2@residuals
acf(rt,main="ACF for residuals with t innovation")
acf(rn,main="ACF for residuals with normal innovation")
acf(rt^2,main="ACF for squared residuals with t innovation")
acf(rn^2,main="ACF for squared residuals with normal innovation")
summary(fit)
summary(fit2)