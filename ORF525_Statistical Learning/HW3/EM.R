# two simple functions
log.sum <- function(v) {
  log.sum.pair <- function(x,y)
  {
    if ((y == -Inf) && (x == -Inf))
    { return(-Inf); }
    if (y < x) return(x+log1p(exp(y-x)))
    else return(y+log1p(exp(x-y)));
  }
  
  r <- v[1];
  for (i in 2:length(v))
    r <- log.sum.pair(r, v[i]);
  return(r);
}

safe.log <- function(v) {
  x = sapply(v, log)
  for(i in 1:length(x)){
    if (x[i] == -Inf)
    {x[i] = -100000}
  }
  if (is.matrix(v)){
    x = matrix(x,dim(v)[1],dim(v)[2]);
  }
  return(x);
}


# Single run of the EM algorithm with spherical gaussian model
sphericalEM <- function(image) {
  
  # Initialize data structures to store parameters
  N    = dim(image)[1]
  ga   = matrix(data = NA, nrow = N, ncol = 5, byrow = FALSE, dimnames = NULL)
  mu   = matrix(data = NA, nrow = 196, ncol = 5, byrow = FALSE, dimnames = NULL)
  si   = vector(length = 5)
  eta  = vector(length = 5)
  
  # Transpose of image matrix
  tIm  = t(image)
  
  # Initialize random mean vectors
  for (j in 1:5) 
    mu[,j]  = sample(0:255, 196)
  
  # Initialize sigma and eta 
  si[] = 1
  eta[] = 0.2
  
  # Initialize loop related variables
  iter = 0
  loglikelihood = Inf
  percChange = Inf
  cat("START")
  # Loop until convergence
  while ( percChange > 0.0001 ) { 
    
    iter <- iter + 1
    #cat(paste("E-Step",'\n'))
    
    # E-Step     
    logData = vector(length = 5)
    for (i in 1:N) {  
      # Calculate log( p( Z_i = j ) * p( x_i | Z_i = j) )
      for (j in 1:5)  
        logData[j] = safe.log(eta[j]) + sum(dnorm(tIm[,i], mu[,j], sqrt(si[j]), log = TRUE))
      
      # Calculate log( p(x_i) )
      denom = log.sum(logData)
      
      # Calculate p( Z_i = j | x_i )
      for (j in 1:5)
        ga[i,j] = exp(logData[j] - denom)
    }
    
    #cat(paste("M-Step")
    
    # M-Step
    # Update new eta
    for (j in 1:5) {
      if (j != 5)
        eta[j] = sum(ga[,j]) / N
      else
        eta[j] = 1 - sum(eta[1:4])
    }
    #cat(paste(eta)
    
    # Update new mu 
    mu = tIm %*% ga
    for (j in 1:5) {
      if ( eta[j] != 0 )
        mu[,j] = mu[,j] / (eta[j] * N)
    }
    
    # Update new sigma
    si[] = 0.01
    for (j in 1:5) {
      for (i in 1:N) 
        si[j] = si[j] + ga[i,j] * ((tIm[,i] - mu[,j]) %*% (tIm[,i] - mu[,j]))[1,1]
    }
    for (j in 1:5) {
      if ( eta[j] != 0 )
        si[j] = si[j] / (eta[j] * N * 196)
    }
    #print(si)   
    
    #print("Calculating fractional change in loglikelihood")
    prevLoglikelihood = loglikelihood
    loglikelihood = 0
    for (i in 1:N) {
      for (j in 1:5) {        
        logData[j] = safe.log(eta[j]) + sum(dnorm(tIm[,i], mu[,j], sqrt(si[j]), log = TRUE))
      }
      loglikelihood = loglikelihood + log.sum(logData)
    }
    if (prevLoglikelihood == Inf)
      percChange = Inf
    else
      percChange = (loglikelihood - prevLoglikelihood) / abs(prevLoglikelihood)
    
    cat(paste(percChange,'\n'))
  }
  
  # assign to x_i to cluster j with largest p( Z_i = j | x_i )
  assignment = vector(length = N)
  for (i in 1:N) {
    for (j in 1:5) {
      if (ga[i,j] == max(ga[i,]))
        assignment[i] = j
    }
  }
  return(list(eta = eta, mu = mu, si = si, assign = assignment, loglikelihood = loglikelihood))
}

# Single run of the EM algorithm with diagonal gaussian model
diagonalEM <- function(image) {
  
  # Initialize data structures to store parameters
  N    = dim(image)[1]
  ga   = matrix(data = NA, nrow = N, ncol = 5, byrow = FALSE, dimnames = NULL)
  mu   = matrix(data = NA, nrow = 196, ncol = 5, byrow = FALSE, dimnames = NULL)
  si   = matrix(data = NA, nrow = 196, ncol = 5, byrow = FALSE, dimnames = NULL)
  eta  = vector(length = 5)
  
  # Transpose of image matrix
  tIm  = t(image)
  
  # Initialize random mean vectors
  for (j in 1:5) 
    mu[,j]  = sample(0:255, 196)
  
  # Initialize sigma and eta 
  si[] = 1
  eta[] = 0.2
  
  # Initialize loop related variables
  iter = 0
  loglikelihood = Inf
  percChange = Inf
  
  
  # Loop until convergence
  while ( percChange > 0.0001 ) { 
    
    print(iter <- iter + 1)
    print("E-Step")
    
    # E-Step     
    logData = vector(length = 5)
    for (i in 1:N) {  
      # Calculate log( p( Z_i = j ) * p( x_i | Z_i = j) )
      for (j in 1:5)  
        logData[j] = safe.log(eta[j]) + sum(dnorm(tIm[,i], mu[,j], sqrt(si[,j]), log = TRUE))
      
      # Calculate log( p(x_i) )
      denom = log.sum(logData)
      
      # Calculate p( Z_i = j | x_i )
      for (j in 1:5)
        ga[i,j] = exp(logData[j] - denom)
    }
    
    print("M-Step")
    
    # M-Step
    # Update new eta
    for (j in 1:5) {
      if (j != 5)
        eta[j] = sum(ga[,j]) / N
      else
        eta[j] = 1 - sum(eta[1:4])
    }
    print(eta)
    
    # Update new mu 
    mu = tIm %*% ga
    for (j in 1:5) {
      if ( eta[j] != 0 )
        mu[,j] = mu[,j] / (eta[j] * N)
    }
    
    # Update new sigma
    si[] = 0.01
    for (i in 1:N) {
       for (j in 1:5) 
        si[,j] = si[,j] + ga[i,j]*(tIm[,i] - mu[,j])^2
    }
    for (j in 1:5) {
      if ( eta[j] != 0 )
        si[,j] = si[,j] / (eta[j] * N)
    }
    
    print("Calculating fractional change in loglikelihood")
    prevLoglikelihood = loglikelihood
    loglikelihood = 0
    for (i in 1:N) {
      for (j in 1:5) {        
        logData[j] = safe.log(eta[j]) + sum(dnorm(tIm[,i], mu[,j], sqrt(si[,j]), log = TRUE))
      }
      loglikelihood = loglikelihood + log.sum(logData)
    }
    if (prevLoglikelihood == Inf)
      percChange = Inf
    else
      percChange = (loglikelihood - prevLoglikelihood) / abs(prevLoglikelihood)
    
    print(percChange)
  }
  
  assignment = vector(length = N)
  # assign to x_i to cluster j with largest p( Z_i = j | x_i )
  for (i in 1:N) {
    for (j in 1:5) {
      if (ga[i,j] == max(ga[i,])) {
        assignment[i] = j
      }
    }
  }
  return(list(eta, mu, si, assignment, loglikelihood))
}

callSphericalEM <- function(image, trials) {
  
  bestObjective = -Inf
  
  # Run sphericalEM many times and select estimates with best loglikelihood
  for (i in 1:trials) {
    output = sphericalEM(image)
    if (output[[5]] > bestObjective) {
      bestEta = output[[1]]
      bestMu = output[[2]]
      bestSi = output[[3]]
      bestAssignment = output[[4]]
      bestObjective = output[[5]]
    }
  }
  bestParam = list(bestEta, bestMu, bestSi)
  return(list(bestParam, bestAssignment, bestObjective))
}

callDiagonalEM <- function(image, trials) {
  
  bestObjective = -Inf
  
  # Run diagonalEM many times and select estimates with best loglikelihood
  for (i in 1:trials) {
    output = diagonalEM(image)
    if (output[[5]] > bestObjective) {
      bestEta = output[[1]]
      bestMu = output[[2]]
      bestSi = output[[3]]
      bestAssignment = output[[4]]
      bestObjective = output[[5]]
    }
  }
  bestParam = list(bestEta, bestMu, bestSi)
  return(list(bestParam, bestAssignment, bestObjective))
}