setwd("C:/Courses/ORF504Financial Econometrics/HW1")
Temp=read.csv("Nasdaqdaily.csv",header=TRUE)
Sdate<-as.Date(Temp$Date)
par(mfrow=c(2,2))
plot(Sdate,Temp$Adj.Close,type="l",xlab="Year",ylab="Daily Price")
closingprice<-Temp[,7]
No<-length(closingprice)
dailyreturn=rep(0,No)
for(i in 2:No)
{
  dailyreturn[i]<-(log(closingprice[i-1])-log(closingprice[i]))
}
plot(Sdate,dailyreturn,type="l",xlab="Year",ylab="Daily Return")

Tempw=read.csv("Nasdaqweekly.csv",header=TRUE)
Sdatew<-as.Date(Tempw$Date)
closingpricew<-Tempw[,7]
Now<-length(closingpricew)
weeklyreturn=rep(0,Now)
for(i in 2:Now)
{
  weeklyreturn[i]<-(log(closingpricew[i-1])-log(closingpricew[i]))
}
plot(Sdatew,weeklyreturn,type="l",xlab="Year",ylab="Weekly Return")

Tempm=read.csv("Nasdaqmonthly.csv",header=TRUE)
Sdatem<-as.Date(Tempm$Date)
closingpricem<-Tempm[,7]
Nom<-length(closingpricem)
monthlyreturn=rep(0,Nom)
for(i in 2:Nom)
{
  monthlyreturn[i]<-(log(closingpricem[i-1])-log(closingpricem[i]))
}
plot(Sdatem,monthlyreturn,type="l",xlab="Year",ylab="Monthly Return")