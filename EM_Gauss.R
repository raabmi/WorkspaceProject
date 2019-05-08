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
  # pjk - occurence probabilty of a jth bin in the kth density
  # njk - vector number of expected observation in jth bin under kth density
  # mu - vector of mean of normals
  # sigma2 - vector of variance of normals
  #OUTPUT
  #likelihood value (loglik)
  
  # TODO term2 and 3
  term1 <- n0 * ln(p0)
  term2 <- 0
  term3 <- 0
  
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
  # njk - vector number of expected observation in jth bin under kth density
  # alpha - numeric value shape of inverse gamma distribution
  # beta  - numeric value rate of inverse gamma distribution
  #OUTPUT
  #likelihood value penalized (loglik)
  
  
  pen <- pinvgamma(sigma, alpha, beta)
  loglikpen <- loglik(n0, p0, J, K, pi, pjk, njk, mu, sigma2) - pen
  
  return(loglikpen)
}

tau <- function(){
  
  
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

#Optimize penalized Logliklihood with optim
optim.loglik <- function(){
  # use function optim
  
  
  return(loglik)
}

# Calculate expected number of observations in bin j nd k
njk <- function(nj, pi, mu, sigma2, a, b,  k){
  # nj - Observed number of observations in bin j
  # mu - vector of mean of normals
  # sigma2- vector of variance of normals
  # pi - vector of mixing proportions of normals
  # k - indizes of kth component
  
  denum <- NULL
  for(i in 1:length(mu)){
   denum <- c(denum,
     pi[i] * pjk(a, b, mu[i], sigma2[i])
   )
   
   num <- nj* pi[k]* pjk(a, b, mu[k], sigma2[k])
   return(num / sum(denum))
  }
}


########################
## EM - Algorithm ######
########################
em.gauss <- function(y, mu, sigma2, pi, espilon=0.000001){
  # y - data numeric vector with observation per bin, 
  # n0  - first value of y must be n0
  # mu - vector of mean of normals
  # sigma2- vector of variance of normals
  # pi - vector of mixing proportions of normals
  # epsilon - stopping criteria (change of likelihood)
  #OUTPUT
  # list(estimated mu, estimated sigma2, liklihood value)
  
  
  #Initialize
  K <- length(mu) # K number of components
  n0 <- y[1] # number of restistent observations
  
  mu_est <- mu
  sigma2_est <- sigma2
  pi_est <- pi
  
  # a = lower bound of bin , b= upper bound
  ab_bin<- data.frame(y=y, a = 5:49, b= 6: 50)
  
  for(i in 1:1000){
    #E- Step
    #Calculate Expected values in bin j under distribution k
    njk_exp <- matrix(0, nrow = 44, ncol= K)
    for(j in 1:44){#do it for each bin (44)
      mjk_exp[j,] <- sapply(1:K, FUN = function(k){
        njk (nj = y[i], 
             pi = pi, 
             mu = mu_est, 
             sigma2 = sigma2_est, 
             a = ab_bin$a[j], 
             b = ab_bin$b[j], 
             k = k)
      }) 
    }
    
    
    #M - Step
    
    # Estimate tau
    tau()
    
    # Estimate pi
    pi()
    
    # Optimize Liklihood (Estimate sigma, mu with optim)
    
  }
  
  return(list(mu= mu_est, sigma2 =sigma2_est, loglik = loglik))
 
}

