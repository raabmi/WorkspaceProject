library(tidyverse)

createCluster <- function(y,k){
  
  #y matrix with columns
  #k number of groups
  
  if(class(y) != "matrix") warning("y is not a matrix")
  if(class(k) != "numeric") warning("k is not a numeric vector")
  if(any(k<0)) warning("only positiv x values are allowed") 
  if(any(y<0)) warning("only positiv y values are allowed")
  
  groups <- round(length(y[,1])/k)
  parameters <- data.frame(mu = c(1:k),sigma2 = c(1:k))
  y <- data.frame(y)
  
  lowerBound <- 0
  upperBound <- groups
  
  # GRUEN: Assign group with rep
  # GRUEN: estimates for pi
  # GRUEN: Deletes 0
  for(i in 1:k){

    yNew <- y %>% filter(y[,1] <= upperBound & lowerBound < y[,1])
    
    parameters[i,1] <- mean(rep(yNew[,1],yNew[,2])) 
    parameters[i,2] <- var(rep(yNew[,1],yNew[,2]))
    
    lowerBound <- upperBound
    upperBound <- upperBound + groups
    
  }
  return(parameters)
}
