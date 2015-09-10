#####################7(table4.4)########################
setwd("C:/Courses/ORF504Financial Econometrics/HW4")
portfolio<-read.table("6pf.txt",header=FALSE)
portfolio<-portfolio[which(portfolio[,1]=="195101"):which(portfolio[,1]=="201012"),2:7]
fama<-read.table("fama.txt",header=FALSE)
fama<-fama[which(fama[,1]=="195101"):which(fama[,1]=="201012"),]
N=6
T=60
T0<-T1<-T3<-pT0<-pT1<-pT3<-rep(0,12)
alpha<-beta<-beta0<-rep(0,N)
res<-matrix(0,T,N)
for(n in 1:12)
{
  begin=T*(n-1)+1
  terminal=T*n
  return<-portfolio[begin:terminal,]
  fm<-fama[begin:terminal,c(2,5)]
  extrareturn<-return-fm[,2]
  for(i in 1:6)
  {
    lmresult=lm(extrareturn[,i]~fm[,1])
    alpha[i]= lmresult$coefficient[1]
    beta[i]= lmresult$coefficient[2]
    res[,i]= lmresult$residuals
  }
  sigma<-matrix(0,N,N)
  for(j in 1:T)
    sigma<-sigma+res[j,]%*%t(res[j,])
  sigma<-1/T*sigma
  marketextra<-mean(fm[,1])
  sigmam<-sd(fm[,1])
  T0[n]=T/(1+marketextra^2/sigmam^2)*t(alpha)%*%solve(sigma)%*%alpha
  T1[n]=(T-N-1)/(N*T)*T0[n]
  T3[n]=(T-N/2-2)*log(1+N*T1[n]/(T-N-1))
}
pT0<-pchisq(T0,N,lower.tail=FALSE)*100
pT1<-pf(T1,N,T-N-1,lower.tail=FALSE)*100
pT3<-pchisq(T3,N,lower.tail=FALSE)*100
write.csv(cbind(T0,pT0,T1,pT1,T3,pT3),file="result4_4.csv")

#####################7(table5.2)########################
portfolio25<-read.table("25pf.txt",header=FALSE)
portfolio25<-portfolio25[which(portfolio25[,1]=="195101"):which(portfolio25[,1]=="201012"),2:26]
N1=6
N2=25
T=60
K=3
pT6<-pT25<-T6<-T25<-rep(0,12)
res6<-res60<-matrix(0,T,N1)
res25<-res250<-matrix(0,T,N2)


for(n in 1:12)
{
  begin=T*(n-1)+1
  terminal=T*n
  return<-portfolio[begin:terminal,]
  fm<-fama[begin:terminal,c(2,3,4,5)]
  extrareturn<-return-fm[,4]
  for(i in 1:6)
  {
    lmresult=lm(extrareturn[,i]~fm[,1]+fm[,2]+fm[,3])
    lmresult0=lm(extrareturn[,i]~fm[,1]+fm[,2]+fm[,3]-1)
    alpha[i]= lmresult$coefficient[1]
    beta[i]= lmresult$coefficient[2]
    beta0[i]= lmresult0$coefficient[2]
    res6[,i]= lmresult$residuals
    res60[,i]= lmresult0$residuals
  }
  sigma<-sigma0<-matrix(0,N1,N1)
  for(j in 1:T){
    sigma<-sigma+res6[j,]%*%t(res6[j,])
    sigma0<-sigma0+res60[j,]%*%t(res60[j,])
  }
  sigma<-1/T*sigma
  sigma0<-1/T*sigma0
  T6[n]<-(T-N1/2-K-1)*(log(det(sigma0))-log(det(sigma)))
  return<-portfolio25[begin:terminal,]
  fm<-fama[begin:terminal,c(2,3,4,5)]
  extrareturn<-return-fm[,4]
  
  for(i in 1:25)
  {
    lmresult=lm(extrareturn[,i]~fm[,1]+fm[,2]+fm[,3])
    lmresult0=lm(extrareturn[,i]~fm[,1]+fm[,2]+fm[,3]-1)
    alpha[i]= lmresult$coefficient[1]
    beta[i]= lmresult$coefficient[2]
    beta0[i]= lmresult0$coefficient[2]
    res25[,i]= lmresult$residuals
    res250[,i]= lmresult0$residuals
  }
  sigma<-sigma0<-matrix(0,N2,N2)
  for(j in 1:T){
    sigma<-sigma+res25[j,]%*%t(res25[j,])
    sigma0<-sigma0+res250[j,]%*%t(res250[j,])
  }
  
  sigma<-1/T*sigma
  sigma0<-1/T*sigma0
  T25[n]<-(T-N1/2-K-1)*(log(det(sigma0))-log(det(sigma)))
}  

pT6<-pchisq(T6,N1,lower.tail=FALSE)*100
pT25<-pchisq(T25,N2,lower.tail=FALSE)*100
write.csv(cbind(T6,pT6,T25,pT25),file="result5_2.csv")
