setwd("C:/Courses/ORF504Financial Econometrics/HW2")
Temp=read.csv("Intelmonthly.csv",header=TRUE)
cp<-Temp[,7]

L<-length(cp)
ttest<-rep(0,L)
row<-rep(0,L)
cpr<-rep(0,L)
for(i in 1:L)
{
cpr[i]<-cp[L+1-i]
}
logprice<-log(cpr)
for (i in 50:L)
{
  xmean2<-sum(logprice[2:i])/(i-1)
  xmean1<-sum(logprice[1:i-1])/(i-1)
 row[i]<-sum((logprice[2:i]-xmean2)* (logprice[1:(i-1)]-xmean1))/sum((logprice[1:i-1]-xmean1)^2)
ttest[i]<-i*(row[i]-1)
}
plot(seq(50,L,1),ttest[50:L],type="l",xlab="Time",ylab="T-test",main="Bubble or not?")