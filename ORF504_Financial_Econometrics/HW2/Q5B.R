n=200
ttest<-rep(0,1000)
ttestd<-rep(0,1000)
for(i in 1:1000)
{
  x<-rep(0,n)
  y<-rep(0,n)
  sam<-rnorm(n)
  x[1]<-sam[1]
  for(j in 2:n)
  {
    x[j]=x[j-1]+sam[j]
  }
  row1<-sum(x[2:n]*x[1:n-1])/sum(x[1:n-1]^2)
  xmean1<-sum(x[1:n-1])/(n-1)
  xmean2<-sum(x[2:n])/(n-1)
  row2=(sum(x[2:n]*x[1:n-1])-xmean1*sum(x[2:n])-xmean2*sum(x[1:n-1])+(n-1)*xmean1*xmean2)/(sum(x[1:n-1]^2)-2*xmean1*sum(x[1:n-1])+(n-1)*xmean1^2)
  ttest[i]=n*(row1-1)
  ttestd[i]=n*(row2-1)
}
quantile(ttest,probs=c(0.01,0.05,0.95,0.99))
quantile(ttestd,probs=c(0.01,0.05,0.95,0.99))