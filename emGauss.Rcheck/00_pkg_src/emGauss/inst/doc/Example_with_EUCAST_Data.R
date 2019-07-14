## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(emGauss)

## ------------------------------------------------------------------------
data('ZD')

## ------------------------------------------------------------------------
# 65, 855, 1091 example datasets 

zd <- as.numeric(ZD[1091, 4:48])
zd.data <- data.frame(bin = 6:50, nrObs = zd)
barplot(zd.data$nrObs, names.arg = c(zd.data$bin))


## ------------------------------------------------------------------------
k <- 5
y <- as.numeric(zd)

result <-em.gauss.opti.groups(y = y,
                     k = k,
                     alpha = 3,
                     beta= 1,
                     method = "quantile",
                     epsilon=0.0001)

## ------------------------------------------------------------------------
plot(result$BIC, ylab = 'BIC',
     xlab = 'Number of components')
abline(v=which.min(result$BIC), lty= 2, col=2)


## ------------------------------------------------------------------------
k <- 2
start.musigma2 <- createCluster(as.matrix(zd.data), k, 
                                method = 'quantile')
start.musigma2


## ------------------------------------------------------------------------
y <- as.numeric(zd)

em.result <- em.gauss(y = y,
                      mu = start.musigma2$mu,
                      sigma2 = start.musigma2$sigma2,
                      pi = rep(1/k, k),
                      alpha = 3,
                      beta = 1,
                      epsilon = 0.0001,
                      max.iter = 650)

## ------------------------------------------------------------------------
plot_fct(y, 
          mu = em.result$mu, 
          sigma2 = em.result$sigma2, 
          pi = em.result$pi, 
          ecoff = em.result$ecoff)

