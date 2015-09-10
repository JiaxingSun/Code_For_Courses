setwd("C:/Courses/ORF504Financial Econometrics/HW1")
Temp=read.csv("Nasdaqdaily.csv",header=TRUE)
Sdate<-as.Date(Temp$Date)
par(mfrow=c(1,3))
closingprice<-Temp[,7]
No<-length(closingprice)
dailyreturn=rep(0,No)
for(i in 2:No)
{
  dailyreturn[i]<-(log(closingprice[i-1])-log(closingprice[i]))
}
qqnorm(dailyreturn,ylab="Quantile of Daily Return",xlab="Normal Quantile")
qqline(dailyreturn,lwd=2,col=2)



Tempw=read.csv("Nasdaqweekly.csv",header=TRUE)
Sdatew<-as.Date(Tempw$Date)
closingpricew<-Tempw[,7]
Now<-length(closingpricew)
weeklyreturn=rep(0,Now)
for(i in 2:Now)
{
  weeklyreturn[i]<-(log(closingpricew[i-1])-log(closingpricew[i]))
}
qqnorm(weeklyreturn,ylab="Quantile of Weekly Return",xlab="Normal Quantile")
qqline(weeklyreturn,lwd=2,col=2)

Tempm=read.csv("Nasdaqmonthly.csv",header=TRUE)
Sdatem<-as.Date(Tempm$Date)
closingpricem<-Tempm[,7]
Nom<-length(closingpricem)
monthlyreturn=rep(0,Nom)
for(i in 2:Nom)
{
  monthlyreturn[i]<-(log(closingpricem[i-1])-log(closingpricem[i]))
}
qqnorm(monthlyreturn,ylab="Quantile of Monthly Return",xlab="Normal Quantile")
qqline(monthlyreturn,lwd=2,col=2)