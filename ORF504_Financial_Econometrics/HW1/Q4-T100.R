Zvec=Q3=Q6=Q12=Qstar3=Qstar6=Qstar12=rep(0,400)
for(i in 1:400)
{
T<-400
X=rt(T,df=5)
Xmean=sum(X)/T
gamma<-rep(0,12)
gamma0<-0
ro<-rep(0,12)
Q<-rep(0,12)
Qstar<-rep(0,12)
Z<-0

#calculate gamma0
for(t in 1:T)
{
  gamma0<-(gamma0+(1/T)*(X[t]-Xmean)*(X[t]-Xmean))
}

#calculate gamma[k],ro[k]
for(k in 1:12)
{
  tempa=sapply(1:(T-k),function(o){(X[o]-Xmean)*(X[o+k]-Xmean)})
  gamma[k]=sum(tempa)/T
  ro[k]=gamma[k]/gamma0
  print("*")
}
Z<-sqrt(T)*ro[1]

#calculate Qstar[m]
for(m in 1:12)
{
  rosq<-ro^2
  rosqp=sapply(1:m,function(o){rosq[o]})
  Qstar[m]<-T*(sum(rosqp))
  print(m)
}

#calculate Q[m]
for(m in 1:12)
{
  rosq<-ro^2
  rosqp=sapply(1:m,function(o){rosq[o]/(T-o)})
  Q[m]<-T*(T+2)*(sum(rosqp))
  print(m)
}
Zvec[i]<-Z
Q3[i]<-Q[3]
Q6[i]<-Q[6]
Q12[i]<-Q[12]
Qstar3[i]<-Qstar[3]
Qstar6[i]<-Qstar[6]
Qstar12[i]<-Qstar[12]
print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
}
par(mfrow=c(2,2))
hist(Zvec,breaks=50,prob=TRUE)
lines(sort(Zvec),dnorm(sort(Zvec),mean=mean(Zvec),sd=sd(Zvec)))
hist(Q3,breaks=50,prob=TRUE)
lines(sort(Q3),dnorm(sort(Q3),mean=mean(Q3),sd=sd(Q3)))
hist(Qstar3,breaks=50,prob=TRUE)
lines(sort(Qstar3),dnorm(sort(Qstar3),mean=mean(Qstar3),sd=sd(Qstar3)))
hist(Q6,breaks=50,prob=TRUE)
lines(sort(Q6),dnorm(sort(Q6),mean=mean(Q6),sd=sd(Q6)))
qnorm(c(0.01,0.05,0.1),mean=0,sd=1)
quantile(abs(Zvec),probs=c(0.01,0.05,0.1))
qchisq(c(0.01,0.05,0.1),df=3)
quantile(abs(Q3),probs=c(0.01,0.05,0.1))
qchisq(c(0.01,0.05,0.1),df=3)
quantile(abs(Qstar3),probs=c(0.01,0.05,0.1))
qchisq(c(0.01,0.05,0.1),df=6)
quantile(abs(Q6),probs=c(0.01,0.05,0.1))
qchisq(c(0.01,0.05,0.1),df=6)
quantile(abs(Qstar6),probs=c(0.01,0.05,0.1))
qchisq(c(0.01,0.05,0.1),df=12)
quantile(abs(Q12),probs=c(0.01,0.05,0.1))
qchisq(c(0.01,0.05,0.1),df=12)
quantile(abs(Qstar12),probs=c(0.01,0.05,0.1))

