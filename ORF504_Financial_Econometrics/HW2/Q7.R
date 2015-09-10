setwd("C:/Courses/ORF504Financial Econometrics/HW2")
Temp=read.csv("mrk.csv",header=TRUE)
cp1<-Temp[,7]
L=length(cp1)
cp<-rep(0,L)
for(i in 1:L)
cp[i]<-cp1[L+1-i]
row=sum(cp[2:L]*cp[1:L-1])/sum(cp[1:L-1]^2)
xmean2=sum(cp[2:L])/(L-1)
xmean1=sum(cp[1:L-1])/(L-1)
rowd=sum((cp[2:L]-xmean2)*(cp[1:L-1]-xmean1))/sum((cp[1:L-1]-xmean1)^2)
test=(row-1)*L
testd=(rowd-1)*L
test
testd
