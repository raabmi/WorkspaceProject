###############################
#####Test njk in E-Step #######
###############################

for(j in 1:44){#do it for each bin
  vapply(1:K,FUN = function(k){
    njk (y[1], pi, mu_est, sigma2_est, a[1], b[1], x)
  },
  FUN.VALUE = 0) 
}

njk (nj = 24, pi = c(0.4, 0.2), 
     mu = c(5, 3), 
     sigma2 = c(5, 1), 
     a = 1 , b = 2 , k = 2)

sapply(1:3,FUN = function(k){
  njk (nj = 10, 
       pi = c(0.4, 0.4, 0.2), 
       mu = c(5, 3, 2), 
       sigma2 = c(5, 1, 4), 
       a = 1, 
       b = 2, 
       k = k)
}) 
