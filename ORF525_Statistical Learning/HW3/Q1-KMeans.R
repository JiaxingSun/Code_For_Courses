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
#################################Q1.1:K-Means################################
#####K-Means++
K<-5#number of clusters
ini<-33#the starting point of K-Means++
cen<-matrix(0,K,196)#this is the 5 cluster centers
cen[1,]<-images2[ini,]
index<-rep(0,K)
index[1]<-ini
index[2]<-which.max(rowSums((t(t(images2)-cen[1,]))^2))
cen[2,]<-images2[index[2],]
index[3]<-which.max(sapply(1:29485,function(k){min(c(sum((t(t(images2[k,])-cen[1,]))^2),sum((t(t(images2[k,])-cen[2,]))^2)))}))
cen[3,]<-images2[index[3],]
index[4]<-which.max(sapply(1:29485,function(k){min(c(sum((t(t(images2[k,])-cen[1,]))^2),sum((t(t(images2[k,])-cen[2,]))^2),sum((t(t(images2[k,])-cen[3,]))^2)))}))
cen[4,]<-images2[index[4],]
index[5]<-which.max(sapply(1:29485,function(k){min(c(sum((t(t(images2[k,])-cen[1,]))^2),sum((t(t(images2[k,])-cen[2,]))^2),sum((t(t(images2[k,])-cen[3,]))^2),sum((t(t(images2[k,])-cen[4,]))^2)))}))
cen[5,]<-images2[index[5],]
par(mfrow=c(2,3))
for(i in 1:5)
  image(restru(images[index[i],],28))
#####Start K-Means
findmin<-function(x)#find the nearest center among the 5
{
  k<-which.min(c(sum((x-cen[1,])^2),sum((x-cen[2,])^2),sum((x-cen[3,])^2),sum((x-cen[4,])^2),sum((x-cen[5,])^2)))
  return(k)
}
numcyc<-30
obj<-rep(0,numcyc)
errate<-rep(0,numcyc)
for(m in 1:numcyc)
{
labels2<-apply(images2,1,findmin)
obj[m]=sum((images2-cen[labels2,])^2)
print(m)
#####define the center of each cluster
for(i in 1:5)
{
  cen[i,]=colMeans(images2[which(labels2==i),])
}
}
#####Plot2
par(mfrow=c(1,5),mar=c(0,0,0,0))
for(i in 1:5)
  image(restru(cen[i,],14),asp=1,col=grey(seq(0,1,length.out=256)))#plot the latest cluster centers
#####Plot1
par(mfrow=c(5,6),mar=c(0,0,0,0))
for(i in 1:5)
  for(m in 1:6)
  image(restru(images2[(which(labels2==i))[m],],14),asp=1,col=grey(seq(0,1,length.out=256)))
#####Plot3
vari<-matrix(0,14,14)
par(mfrow=c(1,5),mar=c(0,0,0,0))
for(i in 1:5)
{
  for(j in which(labels2==i))
    vari=vari+(images2[j,]-cen[i,])^2
  vari=vari/length(which(labels2==i))
  image(restru(vari,14),asp=1,col=grey(seq(0,1,length.out=256)))
}
#####Table1
labmat<-matrix(0,5,5)
labvec<-c(0,2,7,5,6)
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