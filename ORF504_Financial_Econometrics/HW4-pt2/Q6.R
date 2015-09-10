#####################6(a)########################
setwd("C:/Courses/ORF504Financial Econometrics/HW4")
dell<-read.csv("dell.csv",header=TRUE)
dell<-dell[which(dell[,1]=="2001-01-02"):which(dell[,1]=="2008-12-01"),7]
ford<-read.csv("ford.csv",header=TRUE)
ford<-ford[which(ford[,1]=="2001-01-02"):which(ford[,1]=="2008-12-01"),7]
ge<-read.csv("ge.csv",header=TRUE)
ge<-ge[which(ge[,1]=="2001-01-02"):which(ge[,1]=="2008-12-01"),7]
ibm<-read.csv("ibm.csv",header=TRUE)
ibm<-ibm[which(ibm[,1]=="2001-01-02"):which(ibm[,1]=="2008-12-01"),7]
intc<-read.csv("intc.csv",header=TRUE)
intc<-intc[which(intc[,1]=="2001-01-02"):which(intc[,1]=="2008-12-01"),7]
jnj<-read.csv("jnj.csv",header=TRUE)
jnj<-jnj[which(jnj[,1]=="2001-01-02"):which(jnj[,1]=="2008-12-01"),7]
mrk<-read.csv("mrk.csv",header=TRUE)
mrk<-mrk[which(mrk[,1]=="2001-01-02"):which(mrk[,1]=="2008-12-01"),7]
msft<-read.csv("msft.csv",header=TRUE)
msft<-msft[which(msft[,1]=="2001-01-02"):which(msft[,1]=="2008-12-01"),7]
tb<-read.delim("tbill3m.txt",header=FALSE,sep=",")
tb<-tb[,2]/(12*100)  ######this is the risk-free interest rate per month

price<-cbind(dell,ford,ge,ibm,intc,jnj,mrk,msft)
n<-nrow(price)
exreturn<-log(price[2:n,])-log(price[1:n-1,])-tb[1:n-1]
sigma<-var(exreturn)
ksi<-apply(exreturn,2,mean)
A<-sum(solve(sigma)%*%ksi)/0.8
alloc<-solve(sigma)%*%ksi/A

#####################6(b)########################
dell<-read.csv("dell.csv",header=TRUE)
dell<-dell[which(dell[,1]=="2009-01-02"):which(dell[,1]=="2011-01-03"),7]
ford<-read.csv("ford.csv",header=TRUE)
ford<-ford[which(ford[,1]=="2009-01-02"):which(ford[,1]=="2011-01-03"),7]
ge<-read.csv("ge.csv",header=TRUE)
ge<-ge[which(ge[,1]=="2009-01-02"):which(ge[,1]=="2011-01-03"),7]
ibm<-read.csv("ibm.csv",header=TRUE)
ibm<-ibm[which(ibm[,1]=="2009-01-02"):which(ibm[,1]=="2011-01-03"),7]
intc<-read.csv("intc.csv",header=TRUE)
intc<-intc[which(intc[,1]=="2009-01-02"):which(intc[,1]=="2011-01-03"),7]
jnj<-read.csv("jnj.csv",header=TRUE)
jnj<-jnj[which(jnj[,1]=="2009-01-02"):which(jnj[,1]=="2011-01-03"),7]
mrk<-read.csv("mrk.csv",header=TRUE)
mrk<-mrk[which(mrk[,1]=="2009-01-02"):which(mrk[,1]=="2011-01-03"),7]
msft<-read.csv("msft.csv",header=TRUE)
msft<-msft[which(msft[,1]=="2009-01-02"):which(msft[,1]=="2011-01-03"),7]
tbf<-read.delim("tb2.txt",header=FALSE,sep=",")
tbf<-tbf[,2]/(12*100)  ######this is the risk-free interest rate per month
sp500<-read.csv("sp500.csv",header=TRUE)
sp500<-sp500[which(sp500[,1]=="2009-01-02"):which(sp500[,1]=="2011-01-03"),7]
sp500<-log(sp500[2:25])-log(sp500[1:24])

portf<-c(alloc,0.2)#####this is the construction of the portfolio
price<-cbind(dell,ford,ge,ibm,intc,jnj,mrk,msft)
twoy<-log(price[2:25,])-log(price[1:24,])
twoy<-cbind(twoy,tb2)
oney<-twoy[1:12,]
sixm<-twoy[1:6,]

portf2y<-apply(twoy%*%portf,2,mean)
sp2y<-mean(sp500)
portf1y<-apply(oney%*%portf,2,mean)
sp1y<-mean(sp500[1:12])
portf6m<-apply(sixm%*%portf,2,mean)
sp6m<-mean(sp500[1:6])#####this is the comparison of logreturns

vportf2y<-apply(twoy%*%portf,2,sd)
vsp2y<-sd(sp500)
vportf1y<-apply(oney%*%portf,2,sd)
vsp1y<-sd(sp500[1:12])
vportf6m<-apply(sixm%*%portf,2,sd)
vsp6m<-sd(sp500[1:6])#####this is the comparision of volatility

sport2y<-(portf2y-mean(tbf))/vportf2y
ssp2y<-(sp2y-mean(tbf))/vsp2y
sport1y<-(portf1y-mean(tbf))/vportf1y
ssp1y<-(sp1y-mean(tbf[1:12]))/vsp1y
sport6m<-(portf6m-mean(tbf))/vportf6m
ssp6m<-(sp6m-mean(tbf[1:6]))/vsp6m#####this is the comparision of sharpe ratio

#####################6(c)########################
dell<-read.csv("dell.csv",header=TRUE)
dell<-dell[which(dell[,1]=="2008-01-02"):which(dell[,1]=="2008-12-01"),5:7]
ford<-read.csv("ford.csv",header=TRUE)
ford<-ford[which(ford[,1]=="2008-01-02"):which(ford[,1]=="2008-12-01"),5:7]
ge<-read.csv("ge.csv",header=TRUE)
ge<-ge[which(ge[,1]=="2008-01-02"):which(ge[,1]=="2008-12-01"),5:7]
ibm<-read.csv("ibm.csv",header=TRUE)
ibm<-ibm[which(ibm[,1]=="2008-01-02"):which(ibm[,1]=="2008-12-01"),5:7]
intc<-read.csv("intc.csv",header=TRUE)
intc<-intc[which(intc[,1]=="2008-01-02"):which(intc[,1]=="2008-12-01"),5:7]
jnj<-read.csv("jnj.csv",header=TRUE)
jnj<-jnj[which(jnj[,1]=="2008-01-02"):which(jnj[,1]=="2008-12-01"),5:7]
mrk<-read.csv("mrk.csv",header=TRUE)
mrk<-mrk[which(mrk[,1]=="2008-01-02"):which(mrk[,1]=="2008-12-01"),5:7]
msft<-read.csv("msft.csv",header=TRUE)
msft<-msft[which(msft[,1]=="2008-01-02"):which(msft[,1]=="2008-12-01"),5:7]
tb3m<-read.delim("tbill3m.txt",header=FALSE,sep=",")
tb3m<-tb3m[85:96,2]/(12*100) 

sumall<-rbind(dell,ford,ge,ibm,intc,jnj,mrk,msft)
total<-sum(sumall[,1]*sumall[,2])
alloc2<-rep(0,8)
len<-nrow(dell)
for(i in 1:8)
{
  alloc2[i]<-sum(sumall[(len*(i-1)+1):(len*i),1]*sumall[(len*(i-1)+1):(len*i),2])/total
}
portf<-c(alloc2*0.8,0.2)#####this is the construction of the portfolio

portf2y<-apply(twoy%*%portf,2,mean)
sp2y<-mean(sp500)
portf1y<-apply(oney%*%portf,2,mean)
sp1y<-mean(sp500[1:12])
portf6m<-apply(sixm%*%portf,2,mean)
sp6m<-mean(sp500[1:6])#####this is the comparison of logreturns

vportf2y<-apply(twoy%*%portf,2,sd)
vsp2y<-sd(sp500)
vportf1y<-apply(oney%*%portf,2,sd)
vsp1y<-sd(sp500[1:12])
vportf6m<-apply(sixm%*%portf,2,sd)
vsp6m<-sd(sp500[1:6])#####this is the comparision of volatility

sport2y<-(portf2y-mean(tbf))/vportf2y
ssp2y<-(sp2y-mean(tbf))/vsp2y
sport1y<-(portf1y-mean(tbf))/vportf1y
ssp1y<-(sp1y-mean(tbf[1:12]))/vsp1y
sport6m<-(portf6m-mean(tbf))/vportf6m
ssp6m<-(sp6m-mean(tbf[1:6]))/vsp6m#####this is the comparision of sharpe ratio