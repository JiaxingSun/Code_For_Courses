#####################7(table4.4)########################
setwd("C:/Courses/ORF504Financial Econometrics/HW4")
portfolio<-read.table("6pf.txt",header=FALSE)
portfolio<-portfolio[which(portfolio[,1]=="195101"):which(portfolio[,1]=="201012"),2:7]
fama<-read.table("fama.txt",header=FALSE)
fama<-fama[which(fama[,1]=="195101"):which(fama[,1]=="201012"),]
N=6
T=60
T0<-T1<-T3<-rep(0,12)
pT0<-pT1<-pT3<-rep(0,12)
alpha<-beta<-beta0<-rep(0,N)
ee<-matrix(0,T,N)

for(n in 1:12)
{
  start=T*(n-1)+1
  end=T*n
  return<-portfolio[start:end,]
  fm<-fama[start:end,c(2,5)]
  exreturn<-return-fm[,2]
  for(i in 1:6)
  {
    a=lm(exreturn[,i]~fm[,1])
    alpha[i]=a$coefficient[1]
    beta[i]=a$coefficient[2]
    ee[,i]=a$residuals
  }
  sigma<-matrix(0,N,N)
  for(j in 1:T)
   sigma<-sigma+ee[j,]%*%t(ee[j,])
  sigma<-1/T*sigma
  ymbar<-mean(fm[,1])
  sigmam<-sd(fm[,1])
  T0[n]=T/(1+ymbar^2/sigmam^2)*t(alpha)%*%solve(sigma)%*%alpha
  T1[n]=(T-N-1)/(N*T)*T0[n]
  T3[n]=(T-N/2-2)*log(1+N*T1[n]/(T-N-1))
  }
  pT0<-pchisq(T0,N,lower.tail=FALSE)*100
  pT1<-pf(T1,N,T-N-1,lower.tail=FALSE)*100
  pT3<-pchisq(T3,N,lower.tail=FALSE)*100
  result<-cbind(T0,pT0,T1,pT1,T3,pT3)
  write.csv(result,file="result.csv")

#####################7(table5.2)########################
portfolio25<-read.table("25pf.txt",header=FALSE)
portfolio25<-portfolio25[which(portfolio25[,1]=="195101"):which(portfolio25[,1]=="201012"),2:26]
N1=6
N2=25
T=60
K=3
T6<-T25<-rep(0,12)
pT6<-pT25<-rep(0,12)
ee6<-ee60<-matrix(0,T,N1)
ee25<-ee250<-matrix(0,T,N2)


for(n in 1:12)
{
  start=T*(n-1)+1
  end=T*n
  return<-portfolio[start:end,]
  fm<-fama[start:end,c(2,3,4,5)]
  exreturn<-return-fm[,4]
  for(i in 1:6)
  {
    a=lm(exreturn[,i]~fm[,1]+fm[,2]+fm[,3])
    a0=lm(exreturn[,i]~fm[,1]+fm[,2]+fm[,3]-1)
    alpha[i]=a$coefficient[1]
    beta[i]=a$coefficient[2]
    beta0[i]=a0$coefficient[2]
    ee6[,i]=a$residuals
    ee60[,i]=a0$residuals
  }
  sigma<-sigma0<-matrix(0,N1,N1)
  for(j in 1:T){
    sigma<-sigma+ee6[j,]%*%t(ee6[j,])
    sigma0<-sigma0+ee60[j,]%*%t(ee60[j,])
  }
  sigma<-1/T*sigma
  sigma0<-1/T*sigma0
  T6[n]<-(T-N1/2-K-1)*(log(det(sigma0))-log(det(sigma)))
  return<-portfolio25[start:end,]
  fm<-fama[start:end,c(2,3,4,5)]
  exreturn<-return-fm[,4]
  
  for(i in 1:25)
  {
    a=lm(exreturn[,i]~fm[,1]+fm[,2]+fm[,3])
    a0=lm(exreturn[,i]~fm[,1]+fm[,2]+fm[,3]-1)
    alpha[i]=a$coefficient[1]
    beta[i]=a$coefficient[2]
    beta0[i]=a0$coefficient[2]
    ee25[,i]=a$residuals
    ee250[,i]=a0$residuals
  }
  sigma<-sigma0<-matrix(0,N2,N2)
  for(j in 1:T){
    sigma<-sigma+ee25[j,]%*%t(ee25[j,])
    sigma0<-sigma0+ee250[j,]%*%t(ee250[j,])
  }

  sigma<-1/T*sigma
  sigma0<-1/T*sigma0
  T25[n]<-(T-N1/2-K-1)*(log(det(sigma0))-log(det(sigma)))
}  
  
pT6<-pchisq(T6,N1,lower.tail=FALSE)*100
pT25<-pchisq(T25,N2,lower.tail=FALSE)*100
result<-cbind(T6,pT6,T25,pT25)
write.csv(result,file="result5_2.csv")