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
acf(return,main="ACF log-monthly return",col=2)
pacf(return,main="PACF log-monthly return",col=2)
sreturn<-return^2
pacf(sreturn,main="PACF squared log-monthly return",col=2)