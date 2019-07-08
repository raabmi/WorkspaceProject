# Load Librarys
library("rvest")
library("stringr")

# Load Functions
source('createCluster.R')
source('EM_Gauss.R')

#Read Data
ZD <- read.csv("C:/Users/Michaela/Dropbox/Project SS2019/Implementierung/Daten/ZD.csv", sep=";")
ZD <- ZD[!is.na(ZD$ECOFF), ] #Delete with Ecoff = NA

k <- 5
ecoffs <-data.frame(ecoff.zd = ZD$ECOFF, ecoff.calc = 0, num.comp = 0, num.obs = 0)
for(i in 185:368){
  print(i)
  zd <- as.numeric(ZD[i, 4:48])
  y <- as.numeric(zd)
  
  res <-em.gauss.opti.groups(y = y, 
                             k = k, 
                             alpha = 3, 
                             beta= 1, 
                             method = "quantile", 
                             epsilon=0.0001)
  
  ecoffs$ecoff.calc[i] <- res[[which.min(res$BIC)]]$ecoff
  ecoffs$num.comp[i] <- which.min(res$BIC)
  ecoffs$num.obs[i] <- sum(y)
  
  write.csv(ecoffs, file= "ecoffs_results.csv")
  
}


plot(ecoffs$ecoff.zd[1:150], ecoffs$ecoff.calc[1:150])
lines(c(0,50), c(0,50), col= 2, lty =2)
