#####to define a function to restructure a vector into a matrix
restru<-function(x,uni)
{
  k<-length(x)/uni
  result<-matrix(0,k,uni)
  for (i in 1:k)
    result[k-i+1,]=x[(1+uni*(i-1)):(uni*i)]
  return(t(result))
}
#####to define a visulization function to show the first p*q pictures
vis<-function(p,q,y,x)#y is the image file, x is the dim of the images(28 or 14)
{
  par(mfrow=c(p,q))
  for (i in 1:(p*q))
  {
    a1<-restru(y[i,],x)
    image(a1)
  }
}
vis(3,4,images,28)#to visualize the first 12 pictures
#####to shrink images
images2<-matrix(0,dim(images)[1],dim(images)[2]/4)
for(i in 1:dim(images2)[2])
{
  m<-floor((i-1)/14)+1
  n<-i-floor((i-1)/14)*14
  images2[,i]=(images[,(2*m-2)*28+2*n-1]+images[,(2*m-2)*28+2*n]+images[,(2*m-1)*28+2*n-1]+images[,(2*m-1)*28+2*n])/4
}
vis(3,4,images2,14)#to visualize the first 12 pictures
#################################Q1.2:EM################################
#####initialization
images3<-images2[1:2000,]
eta<-rep(0.2,5)
mu<-matrix(1,196,5)
sig<-kronecker(matrix(1,1,5),diag(rep(10,196)))
prob<-matrix(0,2000,5)
den<-rep(0,2000)
for(i in 1:2000)
{
  den[i]=exp(sum(dnorm(images3[i,],mean=mu[,1],sd=sqrt(sig[,(196*1-195):(196*1)]),log=TRUE)))
  print(i)
}
for(i in 1:2000)
{
  for(j in 1:5)
  {
    prob[i,j]<-(eta[j]*exp(-0.5*(t(images3[i,]-mu[,j])%*%inv[,(196*j-195):(196*j)]%*%(images3[i,]-mu[,j]))))/den[i]
  }
  print(i)
}
for(j in 1:5)
{
  eta[j]<-mean(prob[,j])
  mu[,j]<-(t(images3)%*%prob[,j])/sum(prob[,j])
  print(j)
}
for(j in 1:5)
{
  for(i in 1:196)
  {
    sig[i,196*(j-1)+i]<-sum(prob[,j]*(images3[,i]-mu[i,j])^2)/sum(prob[,j])
  }
}
prob[1,]
