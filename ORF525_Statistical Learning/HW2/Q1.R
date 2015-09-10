######################Q1.1##########################
test=1:100
train=101:506
yf=as.matrix(Boston[train,14])
xf=as.matrix(Boston[train,1:13])
y=(yf-mean(yf))/sd(yf)
x=matrix(0,406,13)
for(i in 1:13)
{
  x[,i]=(xf[,i]-mean(xf[,i]))/sd(xf[,i])
}#this is to standardize all the factors 
lasso=glmnet(x, y, alpha=1, nlambda=200, family="gaussian")
plot(lasso, xvar="norm", label=T)
cv1=cv.glmnet(x, y, type.measure="mse", nlambda=200,nfolds=5)
bestfit=which(lasso$lambda==cv1$lambda.min)#find the best lambda
bestfit
abline(v=sum(abs(lasso$beta[,bestfit])),lty=4,col=2,lwd=2)#add a vertical line
plot(cv1)#plot the cross validation

ridge=glmnet(x,y,alpha=0, nlambda=100, family="gaussian", lambda.min.ratio=1e-06)
plot(ridge, xvar="norm", label=T)
cv2=cv.glmnet(x, y, type.measure="mse", alpha=0, nfolds=5,nlambda=100, lambda.min.ratio=1e-06)
bestfit=which(ridge$lambda==cv2$lambda.min)#find the best lambda
bestfit
abline(v=sum(abs(ridge$beta[,bestfit])),lty=4,col=2,lwd=2)#add a vertical line
plot(cv2)#plot the cross validation

y1f=as.matrix(Boston[test,14])
x1f=as.matrix(Boston[test,1:13])
y1=(y1f-mean(y1f))/sd(y1f)
x1=matrix(0,100,13)
for(i in 1:3)
{
  x1[,i]=(x1f[,i]-mean(x1f[,i]))/sd(x1f[,i])
}#this is to standardize all the factors 
for(i in 5:13)
{
  x1[,i]=(x1f[,i]-mean(x1f[,i]))/sd(x1f[,i])
}#this is to standardize all the factors 
p1=predict(ridge, x1, cv2$lambda.min)
mse_ridge<-mean((p1-y1)^2)
mse_ridge# Prediction accuracy
p2=predict(lasso,x1,cv1$lambda.min)
mse_lasso<-mean((p2-y1)^2)
mse_lasso# Prediction accuracy

######################Q1.2##########################

#LASSO

mse=function(beta1,beta2){
  sum((y-beta1*x[,1]-beta2*x[,2])^2)/506
}

y=as.vector(Boston[,"medv"])
x=as.matrix(Boston[,c("nox","rm")])
y=(y-mean(y))/sd(y)
x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])

l1=lm(y~x[,1]+x[,2])#linear regression MLE
summary(l1)

lassoresult=glmnet(x,y,alpha=1, family="gaussian",nlambda=500)
beta1=l1$coefficients[c(2,3)]#get the coefficients beta1 and beta2
threshold=sum(abs(beta1))/2#get the thresholdold as mean absolute value of beta1 and beta2
k=1;
while (sum(abs(lassoresult$beta[,k]))<threshold){
  k=k+1
}#find the biggest index that make lassoresult less than thresholdold
lassoresult$beta[,k]

beta1=seq(-1,1,length.out=100)
beta2=seq(-1,1,length.out=100)
z=matrix(rep(0,10^4),100,100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j]=mse(beta1[i],beta2[j])
  }
}#construct the grid
contour(beta1,beta2,z,asp=1,xlim=c(-1,1))
contour(beta1,beta2,z,levels=c(mse(-0.0268,0.4109)),add=T) 
abline(h=0,lwd=2)
abline(v=0,lwd=2)
points(-0.2390, 0.6231,col=2,pch=16,add=T)
points(-0.0268, 0.4109, col=3, pch=16)
polygon(c(-threshold,0,threshold,0),c(0,threshold,0,-threshold),border=4,lwd=2)




#Ridge

beta1=l1$coefficients[c(2,3)]#get the coefficients beta1 and beta2
threshold=sum(beta1^2)/4#define the thresholdold
ridgeresult=glmnet(x,y,alpha=0, family="gaussian",nlambda=500)
k=1;
while (sum((ridgeresult$beta[,k])^2)<threshold){
  k=k+1
}
k
ridgeresult$beta[,k]

beta1=seq(-1,1,length.out=100)
beta2=seq(-1,1,length.out=100)
z=matrix(rep(0,10^4),100,100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j]=mse(beta1[i],beta2[j])
  }
}#construct the grid

contour(beta1,beta2,z,levels=c(mse(-0.1552413,0.2986470)),asp=1)
contour(beta1,beta2,z,asp=1,add=T)
abline(h=0,lwd=2)
abline(v=0,lwd=2)
points(-0.2390, 0.6231, col=2,pch=16)
points(-0.1552413,0.2986470, col=3, pch=16)
draw.circle(0,0,sqrt(0.1552413^2+0.2986470^2), nv=1000,border="blue", lwd=2)




