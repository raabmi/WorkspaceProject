##################
# Packages ######
#################
library(invgamma)

########################
## Functions ###########
########################

#Probabilty of being in interval j under component k
pjk <- function(a, b, mu, sigma2){
  # a numeric value lower bound of a bin
  # b numeric value upper bound of a bin
  # mu numeric value mean of a normal
  # sigma2 numeric value variance of a normal
  #OUTPUT
  # number - Area unter the density
  
  return(pnorm(b, mu, sqrt(sigma2) ) - 
           pnorm(a, mu, sqrt(sigma2)))
}

# Probabilty of being in interjal j under mixed distribution
pj <- function(p0, pi, a, b, mu , sigma2){
  # p0 - numeric value probability of being in interval B0
  # a numeric value lower bound of a bin
  # b numeric value upper bound of a bin
  # pi numeric vector 
  # mu numeric vector mean of a normal
  # sigma2 numeric vector variance of a normal
  
  correction.term <- 1/(1-p0)
  
  buffer <- NULL
  for (i in length(mu)){
    buffer <- c(buffer, 
                pi[i]*pjk(a, b, mu[i], sigma2[i]))
    
  }
  
  return(1/correction.term * sum(buffer))
  
}


#Loglikelihood
loglik <- function(n0, p0, J, K, pi, pjk, njk, mu, sigma2){
  # n0 - numeric value number of resistent observations
  # p0 - numeric value probability of being in interval B0
  # J -  numeric value Number of Bins
  # K -  numeric value Number of components
  # pi - vector of mixing proportions
  # pjk - matrix (jxk) occurence probabilty of a jth bin in the kth density
  # njk - matrix (jxk) - number of expected observation in jth bin under kth density
  # mu - vector of mean of normals
  # sigma2 - vector of variance of normals
  #OUTPUT
  #likelihood value (loglik)
  
  # TODO term2 and 3
  term1 <- n0 * ln(p0)
  term2 <- sum(colSums(njk) * log(pi))
  term3 <- sum(njk * log(pjk)) #instead of sum sum
  
  #Term 2
  loglik_value <- term1 + term2 + term3
  
  return(loglik_value)
}

#Loglikelihood penalized
loglik.pen <- function(n0, p0, J, K, pi, pjk, njk,mu, sigma2, alpha, beta){
  # n0 - numeric value number of resistent observations
  # p0 - numeric value probability of being in interval B0
  # J -  numeric value Number of Bins
  # K -  numeric value Number of components
  # pi - vector of mixing proportions
  # pjk - occurence probabilty of a jth bin in the kth density
  # njk - matrix number of expected observation in jth bin under kth density
  # alpha - numeric value shape of inverse gamma distribution
  # beta  - numeric value rate of inverse gamma distribution
  #OUTPUT
  #likelihood value penalized (loglik)
  
  
  pen <- sum(log(pinvgamma(sigma, alpha, beta)))
  loglikpen <- loglik(n0, p0, J, K, pi, pjk, njk, mu, sigma2) - pen
  
  return(loglikpen)
}

# Vector of mixing proportions
pi <- function(N, n0, njk){
  # N - numeric value number of all observations
  # n0 - numeric value number of resistance observtions
  # njk - dataframe (jxk) with expected values per bin and component
  #OUTPUT
  # numeric vector with pi for each component k
 
  return(colSums(njk)/(N-n0))
}

#Function for Optimize penalized Logliklihood with optim
optim.loglik.pen <- function(musigma2, n0, p0, J, K, pi, pjk, njk, alpha, beta){
  # musigma, vector with length 2*k, first k = mu, last k= sigma2
  
  mu <- musigma[1:K]
  sigma2 <- musigma[-(1:K)]
  
  loglik.pen <- loglik.pen(n0, p0, J, K, pi, pjk, njk,mu, sigma2, alpha, beta)

  return(loglik.pen)
}

# Calculate expected number of observations in bin j nd k
njk <- function(nj, pi, mu, sigma2, a, b,  k){
  # nj - Observed number of observations in bin j
  # mu - vector of mean of normals
  # sigma2- vector of variance of normals
  # pi - vector of mixing proportions of normals
  # k - indizes of kth component
  #OUTPUT
  # numeric number - expected number of observations
  denum <- NULL
  for(i in 1:length(mu)){
   denum <- c(denum,
     pi[i] * pjk(a, b, mu[i], sigma2[i])
   )

    num <- nj* pi[k]* pjk(a, b, mu[k], sigma2[k])
  }
  return(num / sum(denum))
  
}


########################
## EM - Algorithm ######
########################
em.gauss <- function(y, mu, sigma2, pi, alpha, beta, espilon=0.000001){
  
  # y - data numeric vector with observation per bin, 
  # n0  - first value of y must be n0
  # mu - vector of mean of normals
  # sigma2- vector of variance of normals
  # pi - vector of mixing proportions of normals
  # alpha - numeric number alpha of inverse gauss
  # beta - numeric number beta of inverse gauss
  # epsilon - stopping criteria (change of likelihood)
  #OUTPUT
  # list(estimated mu, estimated sigma2, liklihood value)
  
  if(class(y) != "numeric") warning("y is not a numeric vector")
  if(any(y<0)) warning("only positiv y values are allowed")
  
  if(class(mu) != "numeric") warning("mu is not a numeric vector")
  if(any(mu<0)) warning("only positiv mu values are allowed")
  
  if(class(sigma2) != "numeric") warning("sigma2 is not a numeric vector")
  if(any(sigma2<0)) warning("only positiv sigma2 values are allowed")
  
  if(class(pi) != "numeric") warning("pi is not a numeric vector")
  if(any(pi<0)) warning("only positiv pi values are allowed")
  
  if(length(mu) != length(sigma2) || length(sigma2) != length(pi)){
    warning("mu and sigma2 or sigma2 and pi have not the same length")
  }
  
  if(length(y) <= length(mu)){
    warning("y must be at least the same length as mu")
  } 
  
  if(class(alpha) != "numeric" || length(alpha) != 1 || any(alpha<0)){
    warning("alpha is not a positiv numeric value")
  } 
   
  if(class(beta) != "numeric" || length(beta) != 1 || any(beta<0)){
    warning("beta is not a positiv numeric value")
  }
  
  if(class(espilon) != "numeric" || length(espilon) != 1 || any(espilon<0)){
    warning("espilon is not a positiv numeric value")
  }
  
  #Initialize
  K <- length(mu) # K number of components
  J <- length(y) # J number of Bins
  n0 <- y[1] # number of restistant observations
  N <- sum(y) # Total number of observations
  loglik_prev <- 0 #Initialize previous loglik for first iteration
  delta <- epsilon + 1 #Initialize to go at least one iteration in em_algorithm
  
  
  mu_est <- mu
  sigma2_est <- sigma2
  pi_est <- pi
  
  # a = lower bound of bin , b= upper bound
  ab_bin<- data.frame(y=y, a = 4:49, b= 5: 50)
  
  while(delta > epsilon){
    #E- Step
    #Calculate Expected values in bin j under distribution k
    njk_exp <- matrix(0, nrow = J, ncol= K)
    for(j in 1:J){#do it for each bin (44)
      njk_exp[j,] <- sapply(1:K, FUN = function(k){
        njk (nj = y[j], 
             pi = pi_est, 
             mu = mu_est, 
             sigma2 = sigma2_est, 
             a = ab_bin$a[j], 
             b = ab_bin$b[j], 
             k = k)
      }) 
    }
    
    
    #M - Step
    
    # Estimate pi
    pi_est <- pi(N = N,
                 n0 = n0,
                 njk = njk_exp[-1, ]) #njk has to be without n0
    
    # make a pjk matrix for likelihood
    pjk_exp <- matrix(0, nrow = J, ncol= K)
    for(j in 1:J){#do it for each bin (44)
      pjk_exp[j,] <- sapply(1:K, FUN = function(k){
        pjk(a = ab_bin$a[k],
            b = ab_bin$b[k],
            mu = mu_est[k],
            sigma2 = sigma2_est[k])
      }) 
    }
    # Optimize Likelihood (Estimate sigma, mu with optim)
    # Use as start values estimates from before
    est <- optim(par = c(mu_est, sigma2_est), 
          fn = optim.loglik.pen, 
          n0 = n0, 
          p0 = p0, 
          J = J, 
          K = K, 
          pi = pi_est, 
          pjk = pjk_exp, 
          njk = pjk_exp, 
          alpha= alpha, 
          beta = beta)
    
    mu_est <- est$par[1:K]
    sigma2_est <- est$par[-(1:K)]
    
    loglik_curr <- loglik(n0 = n0, 
                          p0 = p0, 
                          J = J, 
                          K = K, 
                          pi =pi_est, 
                          pjk = pjk_exp, 
                          njk = njk_exp, 
                          mu = mu_est, 
                          sigma2 = sigma2_est) 
    
    delta <- logik_curr - loglik_prev
    
    loglik_prev <- loglik_curr
    
    
  }
  
  return(list(mu= mu_est, sigma2 =sigma2_est, loglik = loglik))
 
}




