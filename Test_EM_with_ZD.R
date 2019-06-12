# Load Librarys
library("rvest")
library("stringr")

# Load Functions
source('createCluster.R')
source('EM_Gauss.R')

#Load Zone Data
ZD <- read.csv("C:/Users/Nicole/Dropbox/Project SS2019/Implementierung/Daten/ZD.csv", sep=";")

#Dataset 3 works, rest not..
#Testdata
# 3 --> works with k= 1 & 2
# 65  --> does not work with k=2, wrong result with k=1 (mu=14, sigma2= 50)
# 855 --> works with k= 2 but strange result (pi = (0,1)) and distribution
# 1091 --> work with k= 2#
# 702

zd <- as.numeric(ZD[65, 4:48])
zd.data <- data.frame(bin = 6:50, nrObs = zd)
zd.data

#View Data
barplot(zd.data$nrObs, names.arg = zd.data$bin)
hist(rep(zd.data$bin, zd.data$nrObs),freq = F, breaks = 30)

#Create Clusters
k <- 2
start.musigma2 <- createCluster(as.matrix(zd.data), k, 
                                method = 'quantile')
start.musigma2

#Do EM Algorithm
source("EM_Gauss.R")
y <- as.numeric(zd)
sink("sink-examp.txt", append = FALSE)
em.result <- em.gauss(y = y,
                      mu = start.musigma2$mu,
                      sigma2 = start.musigma2$sigma2,
                      pi = rep(1/k, k),
                      alpha = 3,
                      beta = 1,
                      epsilon = 0.0001,
                      max.iter = 650)
sink()
em.result


#Plot Results
plot.fct(y = y,
         mu_est = em.result$mu , 
         sigma2_est = em.result$sigma2, 
         pi_est = em.result$pi, 
         ecoff = em.result$ecoff)
  
sum(em.result$pi)

#curve(dinvgamma(x, shape = 3, rate = 0.5), from = 0, to =5)
#dinvgamma(.Machine$double.eps, shape = 3, rate = 0.5)
plot.dens <- function(x, mu, sigma2, pi){
  dens <- 0
  
  for(i in 1:length(mu)){
    dens <- dens + pi[i]* dnorm(x, mean = mu[i], sd = sqrt(sigma2[i]))
  }
  return(dens)
}

hist(rep(zd.data$bin, zd.data$nrObs),freq = F, breaks = 35)
curve(plot.dens(x, 
                em.result$mu, 
                em.result$sigma2, 
                em.result$pi),
      from= 6, to = 50, add = T, ylab = 'density')


#Find optimum
source("EM_Gauss.R")

k <- 5
y <- as.numeric(zd)
sink("sink-examp.txt", append = FALSE)

res <-em.gauss.opti.groups(y = y, 
                     k = k, 
                     alpha = 3, 
                     beta= 1, 
                     method = "quantile", 
                     epsilon=0.0001)
sink()
res

hist(rep(zd.data$bin, zd.data$nrObs),freq = F, breaks= 30)
for(i in 1: k){
  em.result <- res[[i]]
  
  curve(plot.dens(x, 
                  em.result$mu, 
                  em.result$sigma2, 
                  em.result$pi),
        from= 6, to = 50, add = T, ylab = 'density',
        col= i)
  
}
legend('topleft', legend = 1:k, lty= 1, col= 1:k,
       ncol = 2)
res

#AIC
plot(res$AIC, ylab = 'AIC')
abline(v=which.min(res$AIC), lty= 2, col=2)
#BIC
plot(res$BIC, ylab = 'BIC')
abline(v=which.min(res$BIC), lty= 2, col=2)
