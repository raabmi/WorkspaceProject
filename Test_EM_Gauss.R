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
     mu = c(1, 2), 
     sigma2 = c(5, 1), 
     a = 2 , b = 3 , k = 1)

sapply(1:3,FUN = function(k){
  njk (nj = 10, 
       pi = c(0.4, 0.4, 0.2), 
       mu = c(5, 3, 2), 
       sigma2 = c(5, 1, 4), 
       a = 1, 
       b = 2, 
       k = k)
}) 

y <- c(10, 5, 4, 3, 6)
njk_exp <- matrix(0, nrow = 5, ncol= 3)
for(j in 1:5){#do it for each bin (44)
  njk_exp[j,] <- sapply(1:3, FUN = function(k){
    njk (nj = y[j], 
         pi = c(0.4, 0.4, 0.2), 
         mu = c(1, 4, 6), 
         sigma2 = c(1, 2, 2), 
         a = 2, 
         b = 3, 
         k = k)
  }) 
}
njk_exp

pi(N = sum(y),
   n0 = y[1],
   njk = njk_exp[ -1, ])

colSums(njk_exp)

## Test pjk
pjk(a = -10000, 
    b= 2, 
    mu = 2, 
    sigma2 = 1) # should be 0.5

#Test calculate pjk_exp
y <- c(10, 5, 4, 3, 6)
mu_est = c(1, 4, 6) 
sigma2_est = c(1, 2, 2)
ab_bin<- data.frame(y=y, a = 4:8, b= 5: 9)
pjk_exp <- matrix(0, nrow = 5, ncol= 3)
for(j in 1:5){#do it for each bin (44)
  pjk_exp[j,] <- sapply(1:3, FUN = function(k){
    pjk(a = ab_bin$a[j],
        b = ab_bin$b[j],
        mu = mu_est[k],
        sigma2 = sigma2_est[k])
  }) 
}

# Test optim
test_fun <- function (mu, sigma, x1, y2, z3){
  return(z*mu^2 - sigma + x+ y *z)
}
test <- function (xy, z){
  x <- xy[1]
  y <- xy[2]
  return(x^2 - y +z)
}
optim(par = c(1, 2), fn = test,  z = 1)

c(mu, sigma)
optim(c(1), 
      fn =test_fun,
      method = 'Brent',
      lower = -Inf, upper = Inf,
      control = list(x1 = 1, y2= 2, z3 =3, sigma = 1))

test_fun ( 2, 3, 1, 2, 3)
