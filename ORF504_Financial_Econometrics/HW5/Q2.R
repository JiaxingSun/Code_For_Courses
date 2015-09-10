setwd("C:/Course/ORF504Financial Econometrics/HW5")
Earnings.tot = read.csv("SP500Earnings.csv",header = TRUE)
Dividend.tot = read.csv("SP500dividend.csv",header = TRUE)
Dates.tot = Dividend.tot[,1]

start = which(Dates.tot=="1/1/1927")
end = which(Dates.tot=="9/1/2010")

Earnings.tot = Earnings.tot[1:end,2]
Price.tot = Dividend.tot[1:end,2]
Dividend.tot = Dividend.tot[1:end,4]

PEratio.tot = Price.tot/Earnings.tot
logDP.tot = log(Dividend.tot/Price.tot)

####### Question (a)
Dates = strptime(Dates.tot[start:end], "%m/%d/%Y")
plot(Dates, PEratio.tot[start:end], "l", xlab="Time", ylab="PE Ratio", main="PE ratios",col=2)
####### Question (b)
Period1 = which(Dates.tot=="1/1/1927"):which(Dates.tot=="12/1/1950")
Period2 = which(Dates.tot=="1/1/1951"):which(Dates.tot=="12/1/1980")
Period3 = which(Dates.tot=="1/1/1981"):which(Dates.tot=="9/1/2010")

beta = matrix(0,3,6)
Rsq = matrix(0,3,6)
t.stat = matrix(0,3,6)
par(mfrow=c(3,2), mar=c(2, 4, 2, 3))

for(m in 3:3) {
  if(m==1) Period = Period1
  if(m==2) Period = Period2
  if(m==3) Period = Period3	
  
  PEratio = PEratio.tot[Period]
  Price = Price.tot[Period]	
  Return = log(Price[2:length(Price)]/Price[1:(length(Price)-1)])
  
  Klist = c(1,3,12,24,36,48)
  
  for(j in 1:6){
    K = Klist[j]
    T =length(Return)-K+1
    
    ReturnTH = numeric(T)
    for(t in 1:T){
      ReturnTH[t] = sum(Return[t:(t+K-1)])
    }
    PEratioTH = PEratio[1:T]
    
    fit = lm(ReturnTH ~ PEratioTH)
    eps = fit$res
    
    Rsq[m,j] = summary(fit)$r.squared
    beta[m,j] = fit$coeff[2]
    
    X = cbind(1,PEratioTH)             # Newey-West Estimator
    S_T = 1/T * t(X) %*% X
    S_T_0 = matrix(0,2,2)
    for(t in 1:T){
      S_T_0 =S_T_0 + 2*eps[t]^2*(X[t,] %*% t(X[t,]))
    }
    S_T_0 = S_T_0/T
    S_T_1 = matrix(0,2,2)
    for(t in 1:(T-1)){
      S_T_1 =S_T_1 + eps[t]*eps[t+1]*(X[t,]%*%t(X[t+1,])+X[t+1,]%*%t(X[t,]))
    }
    S_T_1 = S_T_1/T		
    Var = 1/T*solve(S_T)%*%(S_T_0 + 0.5*S_T_1)%*%solve(S_T)
    
    t.stat[m,j] = beta[m,j]/sqrt(Var[2,2])		
    
    plot(PEratioTH, ReturnTH, pch = 1, col=2,main = paste(K,"-month", sep=""), ylab="Return", xlab="")
    lines(PEratioTH, fit$fitted)
    fit
  }
}