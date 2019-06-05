library(tidyverse)

createCluster <- function(y,k, method = "quantile"){
  
  #y matrix with columns (name of bin, number of observation)
  #k number of groups
  # method quantiles and binbased 
  
  if(class(y) != "matrix") warning("y is not a matrix")
  if(class(k) != "numeric") warning("k is not a numeric vector")
  if(any(k<0)) warning("only positiv x values are allowed") 
  if(any(y<0)) warning("only positiv y values are allowed")
  
  # Delete 0 Observations
  y <- y[-1, ]
  y <- y[y[,2] != 0, ]
  
  parameters <- data.frame(mu = c(1:k),sigma2 = c(1:k))
  
  
  if (method == 'binbased'){
    groups <- round(length(y[,1])/k)
    y <- data.frame(y)
    
    
    lowerBound <- 0
    upperBound <- groups
    
    # GRUEN: Assign group with rep
    # GRUEN: estimates for pi
    # GRUEN: Delete 0
    for(i in 1:k){
      
      yNew <- y %>% filter(y[,1] <= upperBound & lowerBound < y[,1])
      
      parameters[i,1] <- mean(rep(yNew[,1],yNew[,2])) 
      parameters[i,2] <- var(rep(yNew[,1],yNew[,2]))
      
      lowerBound <- upperBound
      upperBound <- upperBound + groups
      
    }
    
  }
  
  if ( method == "quantile"){
    groups <- round(sum(y[,2])/k)
    
    y.splitted <- rep(y[,1],y[,2])
    g <- rep(1:k, each= groups)
    
    df <- data.frame(d = y.splitted, group = g[1:length(y.splitted)] )
    parameters[,1] <- aggregate(df$d, list(df$group), FUN = 'mean')$x
    parameters[,2] <- aggregate(df$d, list(df$group), FUN = 'var')$x
    
  }
 
 
  return(parameters)
}
