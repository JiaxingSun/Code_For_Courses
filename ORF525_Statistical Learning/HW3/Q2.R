###########################Q2.1##############################
setwd("C:/Course/ORF525Statistical Learning/images")
fn=list.files("C:/Course/ORF525Statistical Learning/images",full.names=T)
X=matrix(0,20,117300)
for ( i in 1:20)
  X[i,]=as.vector(readPNG(fn[[i]])[171:400,206:375,])
X=X-rep(1,20)%*%t(colMeans(X))
r=eigen(X%*%t(X)/20)
D=r$values
U=r$vectors
###########################Q2.2##############################
#####Plot1:scree graph
plot(D,ylab="Eigenvalues",main="Scree graph of eigenvalues")
#####Plot2:largest 6 eigen-faces
numei<-20#number of eigenfaces checked
Vec<-matrix(0,117300,numei)
SVec<-matrix(0,117300,numei)

for (i in 1:numei){
  Vec[,i]=t(X)%*%U[,i]/sqrt(20*D[i]);
  SVec[,i]=(Vec[,i]-min(Vec[,i]))/(max(Vec[,i])-min(Vec[,i]));
  imme=array(SVec[,i],dim=c(230,170,3));
  filename=paste("eigen_face",i,".png",sep="")
  writePNG(imme,target=filename);
}
#####Plot3:using only the first 2 eigenfaces
nume<-10#number of eigenfaces used
eta<-matrix(0,nume,20)
for(i in 1:20)
{
  for(j in 1:nume)
  {
    eta[j,i]=t(Vec[,j])%*%X[i,]
  }
}
Xsim=matrix(0,20,117300)#use only the first 2 eigen face
SXsim=matrix(0,20,117300)#this is the scaled version of Xsim
for(i in 1:20)
{
  Xsim[i,]=rowSums(sapply(1:nume,function(o){eta[o,i]*Vec[,o]}))
  SXsim[i,]=(Xsim[i,]-min(Xsim[i,]))/(max(Xsim[i,])-min(Xsim[i,]))
  imme=array(SXsim[i,],dim=c(230,170,3));
  filename=paste(nume,"_eigenface_used",i,".png",sep="")
  writePNG(imme,target=filename);
}
#####Plot4:Pairwise similarity
simi<-matrix(0,20,20)
for(i in 1:20)
{
  for(j in 1:20)
  {
    simi[i,j]=sum(sapply(1:6,function(o){(eta[o,i]-eta[o,j])^2}))
  }
}
plot(colSums(simi),xlab="contestant",ylab="salient")