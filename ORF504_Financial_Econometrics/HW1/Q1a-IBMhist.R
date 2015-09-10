setwd("C:/Courses/ORF504Financial Econometrics/HW1")
Temp=read.csv("IBMdaily.csv",header=TRUE)
Sdate<-as.Date(Temp$Date)
par(mfrow=c(1,3))
closingprice<-Temp[,7]
No<-length(closingprice)
dailyreturn=rep(0,No)
for(i in 2:No)
{
  dailyreturn[i]<-(log(closingprice[i-1])-log(closingprice[i]))
}
hist(dailyreturn,ylab="Density",breaks=50,prob=TRUE)
lines(sort(dailyreturn),dnorm(sort(dailyreturn),mean=mean(dailyreturn),sd=sd(dailyreturn)),lwd=2,col=3)


Tempw=read.csv("IBMweekly.csv",header=TRUE)
Sdatew<-as.Date(Tempw$Date)
closingpricew<-Tempw[,7]
Now<-length(closingpricew)
weeklyreturn=rep(0,Now)
for(i in 2:Now)
{
  weeklyreturn[i]<-(log(closingpricew[i-1])-log(closingpricew[i]))
}
hist(weeklyreturn,ylab="Density",breaks=50,prob=TRUE)
lines(sort(weeklyreturn),dnorm(sort(weeklyreturn),mean=mean(weeklyreturn),sd=sd(weeklyreturn)),lwd=2,col=3)

Tempm=read.csv("IBMmonthly.csv",header=TRUE)
Sdatem<-as.Date(Tempm$Date)
closingpricem<-Tempm[,7]
Nom<-length(closingpricem)
monthlyreturn=rep(0,Nom)
for(i in 2:Nom)
{
  monthlyreturn[i]<-(log(closingpricem[i-1])-log(closingpricem[i]))
}
hist(monthlyreturn,ylab="Density",breaks=50,prob=TRUE)
lines(sort(monthlyreturn),dnorm(sort(monthlyreturn),mean=mean(monthlyreturn),sd=sd(monthlyreturn)),lwd=2,col=3)