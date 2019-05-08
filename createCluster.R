library(tidyverse)

createCluster <- function(y,k){
  
 #todo warning message
 
  #y matrix with columns
  #k number of groups
  
  groups <- round(length(y[,1])/k)
  parameters <- data.frame(mu = c(1:k),sigma2 = c(1:k))
  y <- data.frame(y)
  
  lowerBound <- 0
  upperBound <- groups
  
  for(i in 1:k){

    yNew <- y %>% filter(y[,1] <= upperBound & lowerBound < y[,1])
    
    parameters[i,1] <- mean(rep(yNew[,1],yNew[,2])) 
    parameters[i,2] <- var(rep(yNew[,1],yNew[,2]))
    
    lowerBound <- upperBound
    upperBound <- upperBound + groups
    
  }
  return(parameters)
}
