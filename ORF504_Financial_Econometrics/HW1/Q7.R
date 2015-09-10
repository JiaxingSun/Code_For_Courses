par(mfrow=c(1,2))
x<-rep(0,501)
epi<-rnorm(501,mean=0,sd=1)
for(t in 2:501)
{
  x[t]=0.8*epi[t-1]^2/(1+epi[t-1]^2)+epi[t]
}
acf(x,main="ACF")
pacf(x,main="PACF")