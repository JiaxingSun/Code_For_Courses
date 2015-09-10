T<-100
X=rnorm(T,mean=0,sd=1)
Xmean=sum(X)/T
gamma<-rep(0,12)
gamma0<-0
ro<-rep(0,12)
Q<-rep(0,6)
Qstar<-rep(0,6)

#calculate gamma0
for(t in 1:T)
{
  gamma0<-(gamma0+(1/T)*(X[t]-Xmean)*(X[t]-Xmean))
}

#calculate gamma[k],ro[k]
for(k in 1:12)
{
  for(t in 1:T-k)
  {
    gamma[k]<-(gamma[k]+(1/T)*(X[t]-Xmean)*(X[t+k]-Xmean))
    print("*")
  }
  ro[k]=gamma[k]/gamma0
}

for(m in 1:6)
{
  rosq<-ro^2
  rosqp=sapply(1:m,function(o){rosq[o]})
  Qstar[m]<-T*(sum(rosqp))
}

for(m in 1:6)
{
  rosq<-ro^2
  rosqp=sapply(1:m,function(o){rosq[o]/(T-o)})
  Q[m]<-T*(T+2)*(sum(rosqp))
}

print(Q)
print(Qstar)