setwd("C:/Courses/ORF504Financial Econometrics/HW1")
Temp=read.csv("Nasdaqdaily.csv",header=TRUE)
Sdate<-as.Date(Temp$Date)
par(mfrow=c(3,3))
closingprice<-Temp[,7]
No<-length(closingprice)
dailyreturn=rep(0,No)
for(i in 2:No)
{
  dailyreturn[i]<-(log(closingprice[i-1])-log(closingprice[i]))
}
acf(dailyreturn,main="Daily Returns")
acf(dailyreturn^2,main="Squared Daily Returns")
acf(abs(dailyreturn),main="Abs Daily Returns")



Tempw=read.csv("Nasdaqweekly.csv",header=TRUE)
Sdatew<-as.Date(Tempw$Date)
closingpricew<-Tempw[,7]
Now<-length(closingpricew)
weeklyreturn=rep(0,Now)
for(i in 2:Now)
{
  weeklyreturn[i]<-(log(closingpricew[i-1])-log(closingpricew[i]))
}
acf(weeklyreturn,main="Weekly Returns")
acf(weeklyreturn^2,main="Squared Weekly Returns")
acf(abs(weeklyreturn),main="Abs Weekly Returns")

Tempm=read.csv("Nasdaqmonthly.csv",header=TRUE)
Sdatem<-as.Date(Tempm$Date)
closingpricem<-Tempm[,7]
Nom<-length(closingpricem)
monthlyreturn=rep(0,Nom)
for(i in 2:Nom)
{
  monthlyreturn[i]<-(log(closingpricem[i-1])-log(closingpricem[i]))
}
acf(monthlyreturn,main="Monthly Returns")
acf(monthlyreturn^2,main="Squared Monthly Returns")
acf(abs(monthlyreturn),main="Abs Monthly Returns")