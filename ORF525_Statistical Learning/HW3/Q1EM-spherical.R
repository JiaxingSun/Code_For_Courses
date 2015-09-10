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
#################################Q1.2:EM-diagonal covariance################################
#####initialization
images3<-images2[1:29485,]+matrix(rnorm(29485*196,0.1,0.1),29485,196)
eta<-c(0.1,0.2,0.4,0.2,0.1)
mu<-matrix(50,196,5)
sig<-matrix(10000,196,5)
prob<-matrix(0,29485,5)
for(n in 1:29485)
{
  if(labels[n]==0)
    prob[n,1]=1
  if(labels[n]==2)
    prob[n,2]=1
  if(labels[n]==5)
    prob[n,3]=1
  if(labels[n]==6)
    prob[n,4]=1
  if(labels[n]==7)
    prob[n,5]=1
}

ele<-matrix(0,29485,5)
proberr<-rep(0,10000)
sigerr<-rep(0,10000)
muerr<-rep(0,10000)

for(m in 1:30)
{
  probt<-prob
  sigt<-sig
  mut<-mu
  for(j in 1:5)
  {
    eta[j]<-mean(prob[,j])
    mu[,j]<-(t(images3)%*%prob[,j])/sum(prob[,j])
  }
  
  for(j in 1:5)
  {
    sig[,j]<-sum(prob[,j]*colSums((t(images3)-mu[,j])^2))/(sum(prob[,j])*196)
  }
  
  
  for(i in 1:29485)
  {
    for(j in 1:5)
    {
      ele[i,j]<-log(eta[j])+sum(dnorm(images3[i,],mu[,j],sqrt(sig[,j]),log=T))
    }
    ele[i,]<-ele[i,]-max(ele[i,])#####all the ele are 0 now, to be fixed
    prob[i,]<-exp(ele[i,])/sum(exp(ele[i,]))
  }   
  proberr[m]<-sum((probt-prob)^2)
  sigerr[m]<-sum((sigt-sig)^2)
  muerr[m]<-sum((mut-mu)^2)
  print(m)
}

#####label all the pictures
labels2<-rep(0,29485)
for(i in 1:29485)
  labels2[i]<-which.max(prob[i,])
#####Plot1
par(mfrow=c(5,6),mar=c(0,0,0,0))
for(i in 1:5)
  for(m in 1:6)
    image(restru(images2[(which(labels2==i))[m+13],],14),asp=1,col=grey(seq(0,1,length.out=256)))
#####Plot2
par(mfrow=c(1,5),mar=c(0,0,0,0))
cente<-matrix(0,196,5)
for(i in 1:5)
{
  summat<-rep(0,196)
  for(j in 1:length(which(labels2==i)))
    summat=summat+images2[which(labels2==i)[j],]
  cente[,i]<-summat/length(which(labels2==i))
  image(restru(cente[,i],14),asp=1,col=grey(seq(0,1,length.out=256)))    
}
#####Plot3
vari<-matrix(0,14,14)
par(mfrow=c(1,5),mar=c(0,0,0,0))
for(i in 1:5)
{
  for(j in which(labels2==i))
    vari=vari+(images2[j,]-cente[,i])^2
  vari=vari/length(which(labels2==i))
  image(restru(vari,14),asp=1,col=grey(seq(0,1,length.out=256)))
}
#####Table1
labmat<-matrix(0,5,5)
labvec<-c(0,2,5,6,7)
for(i in 1:5)#corresponds to real labels
{
  for(j in 1:5)#corresponds to assigned labels
  {
    labmat[i,j]=sum(labels2[which(labels==labvec[i])]==j)
  }
}
#####calculate the error rate
era<-rep(0,5)
for(i in 1:5)
{
  era[i]=1-labmat[i,i]/sum(labmat[,i])
}
