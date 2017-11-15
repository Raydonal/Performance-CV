# ---------------------------------------------------------------------------------------
# --------------- Reproduciblre Research  -----------------------------------------------
# Script to reproduce the results on paper: 
# Performance of several estimators of the coefficient of 
# variation under distributional specification of the data
#
# Created by Raydonal Ospina
# Modified by: Raydonal Ospina
# We are based on old Jorge Velez implementation to core distributions
#
# Raydonal  15/11/2017
# Contact: raydonal@de.ufpe.br
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#### Functions 
# ---------------------------------------------------------------------------------------
## median
median <- function(x) as.numeric(quantile(x, probs = .5))
median <- cmpfun(median)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
## classical coefficient of variation
old <- function(x) sd(x)/mean(x)
old <- cmpfun(old) 
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
## edith
edith <- function(x){
  q <- quantile(x, probs = c(.25, .75))
  as.numeric((q[2] - q[1]) / (q[2] + q[1]))
}
edith <- cmpfun(edith)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
## edith type 8
edith8 <- function(x){
  q <- quantile(x, probs = c(.25, .75), type = 8)
  as.numeric((q[2] - q[1]) / (q[2] + q[1]))
}
edith <- cmpfun(edith)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
## mad/median
madmedian <- function(x){
  mdn <- median(x)
  MAD <- 1.4826 * median(abs(x - mdn))
  MAD/mdn
}
madmedian <- cmpfun(madmedian)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
## mnad/median
MnADmedian <- function(x){
  mdn <- median(x)
  MnAD <- mean(abs(x - mdn))
  MnAD/mdn
}
MnADmedian <- cmpfun(MnADmedian)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
## all together
cvs <- function(x)  c(old(x), edith(x), edith8(x), madmedian(x), MnADmedian(x))
cvs <- cmpfun(cvs)
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
## Monte Carlo Simulation

## Parameters of simulation

## random seed
 set.seed(121)

# Sample size
n <- c(10, 25, 50, 100, 200) 

# mean
mu <- c(0.1, 0.4, 0.7, 1, 5,  15,  30)

# dispersion
s <- c(.1, 0.3, 0.6, 1, 3, 5) 


## number of Monte Carlo replicates
B <- 10000
# ---------------------------------------------------------------------------------------


ptm <- proc.time()


#########################################################################################
## ---------------------------------------------------------------------------------------
##  Binomial case
# E(X) = m*p = mu
# Var(X) = m*p*(1-p) = sigma^2 =>
# p = 1-(sigma^2/ mu)
# m = [mu / p],   [.] is the integer part function  
## ---------------------------------------------------------------------------------------


# function p as mu and s function
func.p <- function(m,s) {1 -(s^2/m)}

# Probability with valid restrictio 0 < s^2/mu < 1 
p <- outer(mu, s, Vectorize(func.p))
# True value of p 
p <- unique(sort(p[p>0]))

# m experiments to exit - Binomial(m,p)
func.m <- function(m, p) {floor(mu/p)}

# Probability with valid restriction 0 < s^2/mu < 1 
m <- unique(sort(outer(mu, p, func.m)))
m <- m[m>1]

# Grid parameter - Binomial distribution
# params <- expand.grid(m = m, p = p)

params <- expand.grid(n = n, m = m, p = p)

# true parameter value CV
params$true <- with(params, sqrt(m*p*(1-p)) / (m*p))

save(params, file="Binomial-params.Rdata")

# Monte Carlo Scheme
L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
for(i in 1:NROW(params)){
  r <- as.numeric(unlist(params[i,]))
  ## replicates 
  res <- parallel:::mclapply(1:B, function(i){
  x <- rbinom(n = r[1], size = r[2], p = r[3])
  cvs(x)
  }, mc.cores = 4)
  res <- do.call(rbind, res)
  L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
  if(i %% 5 == 0) cat(i, " ")
  #  if(i == NROW(params)) cat("\n")
}
out <- cbind(params, L)

  # Add the Gamma measures
  out$CQV7=out$edith/out$old
  out$CQV8=out$edith8/out$old
  out$CVMAD=out$MADmedian/out$old
  out$CQVMnAD=out$MnADmedian/out$old
  
  save(out, file="Binomial-Sim-Wide.Rdata")

  data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
  data_long$n=as.factor(data_long$n)
  data_long$m=as.factor(data_long$m)
  data_long$p=as.factor(format(data_long$p, digits=2)  )
  data_long$lgamma = -log10(data_long$gamma)
  data_long$ltrue = -log10(data_long$true)
  
  save(data_long, file="Binomial-Sim-Long.Rdata")
  
#########################################################################################
  
  #########################################################################################
  ## ------------------------
  ##  Uniform 
  # U~U(a,b)
  # E(U) = 0.5*(a+b) = mu
  # Var(U) =  1/12 * (b-a)^2 = sigma^2 =>
  # X~(mu-sigma*sqrt(3), mu+sigma*sqrt(3)) 
  # E(X) = mu
  # VarX = sigma^2
  ## ------------------------
  
  
  # function p as mu and s function
  func.a <- function(m,s) {m - (s*sqrt(3))}
  func.b <- function(m,s) {m + (s*sqrt(3))}
  
  # lim inf
  a <- as.vector(outer(mu, s, Vectorize(func.a)))
  # lim sup
  b <- as.vector(outer(mu, s, Vectorize(func.b)))
  params <- data.frame(a,b)
  params <- cbind(n=sort(rep(n, length(a))), params)
  
  # true parameter value CV
  params$true <- with(params, sqrt((1/12)*(b-a)^2) / (0.5*(a+b)))

  save(params, file="Uniform-params.Rdata")
  
  # Monte Carlo Scheme
  L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
  colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
  for(i in 1:NROW(params)){
    r <- as.numeric(unlist(params[i,]))
    ## replicates
    res <- parallel:::mclapply(1:B, function(i){
      x <- runif(r[1], min =r[2] , max=r[3])
      cvs(x)
    }, mc.cores = 4)
    res <- do.call(rbind, res)
    L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
    if(i %% 5 == 0) cat(i, " ")
    #  if(i == NROW(params)) cat("\n")
  }
  out <- cbind(params, L)
  
  
  # Add the Gamma measures
  out$CQV7=out$edith/out$old
  out$CQV8=out$edith8/out$old
  out$CVMAD=out$MADmedian/out$old
  out$CQVMnAD=out$MnADmedian/out$old
  
  save(out, file="Unifor-Sim-Wide.Rdata")
  
  data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
  data_long$n=as.factor(data_long$n)
  # data_long$m=as.factor(data_long$m)
  # data_long$p=as.factor(format(data_long$p, digits=2)  )
  data_long$lgamma = -log10(data_long$gamma)
  data_long$ltrue = -log10(data_long$true)
  
  save(data_long, file="Uniform-Sim-Long.Rdata")
  
#########################################################################################

#########################################################################################
## ------------------------
##  Normal 
# E(X) = mu
# VarX = sigma^2
## ------------------------  

  params <- expand.grid(n = n, mu = mu, s = s)
  NROW(params)
  params$true <- with(params, s/mu)
 
  save(params, file="Normal-params.Rdata")
  
  L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
  colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
  
  for(i in 1:NROW(params)){
    r <- as.numeric(unlist(params[i,]))
    ## replicates
    res <- parallel:::mclapply(1:B, function(i){
      x <- rnorm(n = r[1], mean = r[2], s = r[3])
      cvs(x)
    }, mc.cores = 4)
    res <- do.call(rbind, res)
    L[i, ] <- colMeans((res-r[4])^2, na.rm = TRUE)
    if(i %% 10 == 0) cat(i, " ")
    #  if(i == NROW(params)) cat("\n")
  }
  out <- cbind(params, L)
  
  # Add the Gamma measures
  out$CQV7=out$edith/out$old
  out$CQV8=out$edith8/out$old
  out$CVMAD=out$MADmedian/out$old
  out$CQVMnAD=out$MnADmedian/out$old
  
  save(out, file="Normal-Sim-Wide.Rdata")
  
  data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
  data_long$n=as.factor(data_long$n)
  # data_long$m=as.factor(data_long$m)
  # data_long$p=as.factor(format(data_long$p, digits=2)  )
  data_long$lgamma = -log10(data_long$gamma)
  data_long$ltrue = -log10(data_long$true)
  
  save(data_long, file="Normal-Sim-Long.Rdata")
  #########################################################################################

  #########################################################################################
  ## ------------------------
  ##  Poisson
  # E(X) = mu 
  # Var(X) = mu^2
  ## ------------------------  
  
  # function p as mu and s function
  func.lambda <- function(m,s) {1/(s/m)^2}
  
  # lambda as function of mu and sigma
  lambda <- unique(sort(outer(mu, s, Vectorize(func.lambda))))
  
  params <- expand.grid(n = n, lambda = lambda)
  params$true <- with(params, {
    mu <- lambda
    s <- sqrt(lambda)
    s/mu
  })
  
  save(params, file="Poisson-params.Rdata")
  
  L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
  colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
    for(i in 1:NROW(params)){
    r <- as.numeric(unlist(params[i,]))
    ## replicates
    res <- parallel:::mclapply(1:B, function(i){
      x <- rpois(n = r[1], lambda = r[2])
      cvs(x)
    }, mc.cores = 4)
    res <- do.call(rbind, res)
    L[i, ] <- colMeans((res-r[3])^2, na.rm = TRUE)
    if(i %% 10 == 0) cat(i, " ")
    #  if(i == NROW(params)) cat("\n")
  }
  out <- cbind(params, L)
  
  # Add the Gamma measures
  out$CQV7=out$edith/out$old
  out$CQV8=out$edith8/out$old
  out$CVMAD=out$MADmedian/out$old
  out$CQVMnAD=out$MnADmedian/out$old
  
  save(out, file="Poisson-Sim-Wide.Rdata")
  
  data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
  data_long$n=as.factor(data_long$n)
  # data_long$m=as.factor(data_long$m)
  # data_long$p=as.factor(format(data_long$p, digits=2)  )
  data_long$lgamma = -log10(data_long$gamma)
  data_long$ltrue = -log10(data_long$true)
  
  save(data_long, file="Poisson-Sim-Long.Rdata")
  
  #########################################################################################
  ## ------------------------
  ##  Beta case 
  ## ------------------------

  
  # function q as (mu,s) functions
  func.qshape <- function(mu,s) {
    mu.star =  mu #boot::inv.logit(mu) #(m - min(mu))/(max(mu)-min(mu))
    s2.star = s^2 #boot::inv.logit(s^2)  #(s - min(s))/(max(s)-min(s))
    p =  mu.star*((mu.star*(1-mu.star)/ s2.star) -1) 
    }
  
  # function p as (mu, s) functions
  func.pshape <- function(mu,s) {
    mu.star =  mu #boot::inv.logit(mu) #(m - min(mu))/(max(mu)-min(mu))
    s2.star = s^2 #boot::inv.logit(s^2)  #(s - min(s))/(max(s)-min(s))
    p =  (1-mu.star)*((mu.star*(1-mu.star)/ s2.star) -1) 
  }
  
  q <- outer(mu, s,  Vectorize(func.qshape))
  # True value of p 
  q <- unique(sort(q[q>0]))
  
  
  p <- outer(mu, s,  Vectorize(func.pshape))
  # True value of p 
  p <- unique(sort(p[p>0]))
  
  params <- expand.grid(n = n, a = p, b = q)
   
  
  params$true <- with(params, {
    mu <- a/(a+b)
    s <- sqrt(a*b/((a+b)^2*(a+b+1)))
    true <- s/mu
  })
  

  
  save(params, file="Beta-params.Rdata")

  L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
  colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
  for(i in 1:NROW(params)){
    r <- as.numeric(unlist(params[i,]))
    ## replicates
    res <- parallel:::mclapply(1:B, function(i){
      x <- rbeta(n = r[1], shape1 = r[2], shape2 = r[3])
      cvs(x)
    }, mc.cores = 4)
    res <- do.call(rbind, res)
    L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
    if(i %% 10 == 0) cat(i, " ")
    #  if(i == NROW(params)) cat("\n")
  }
  out <- cbind(params, L)
  
  
  # Add the Gamma measures
  out$CQV7=out$edith/out$old
  out$CQV8=out$edith8/out$old
  out$CVMAD=out$MADmedian/out$old
  out$CQVMnAD=out$MnADmedian/out$old
  
  save(out, file="Beta-Sim-Wide.Rdata")
  
  data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
  data_long$n=as.factor(data_long$n)
  # data_long$m=as.factor(data_long$m)
  # data_long$p=as.factor(format(data_long$p, digits=2)  )
  data_long$lgamma = -log10(data_long$gamma)
  data_long$ltrue = -log10(data_long$true)
  
  save(data_long, file="Beta-Sim-Long.Rdata")
  
  #########################################################################################
  
  
  #######################################################################################
  # ------------------------
  #  Exponential
  # ------------------------
  # E(X) = 1/lambda 
  # Var(X) = 1/lambda^2
  ## ------------------------  
  
  # function lamda as mu and s function - Using the idea of Poisson distribution
  func.lambda <- function(m,s) {1/(s/m)}
  
  # lambda as function of mu and sigma
  lambda <- unique(sort(outer(mu, s, Vectorize(func.lambda))))
  
  params <- expand.grid(n = n, lambda = lambda)
  params$true <- with(params, {
    mu <- lambda
    s <- lambda
    s/mu
  })
  
  save(params, file="Exponentil-params.Rdata")
  
  L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
  colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
  for(i in 1:NROW(params)){
    r <- as.numeric(unlist(params[i,]))
    ## replicates
    res <- parallel:::mclapply(1:B, function(i){
      x <- rexp(r[1], 1/r[2])
      cvs(x)
    }, mc.cores = 4)
    res <- do.call(rbind, res)
    L[i,] <- colMeans((res-1)^2, na.rm = TRUE)
    if(i %% 5 == 0) cat(i, " ")
    #  if(i == NROW(params)) cat("\n")
  }
  out <- cbind(params, L)
  
  # Add the Gamma measures
  out$CQV7=out$edith/out$old
  out$CQV8=out$edith8/out$old
  out$CVMAD=out$MADmedian/out$old
  out$CQVMnAD=out$MnADmedian/out$old
  
  save(out, file="Exponential-Sim-Wide.Rdata")
  
  data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
  data_long$n=as.factor(data_long$n)
  # data_long$m=as.factor(data_long$m)
  # data_long$p=as.factor(format(data_long$p, digits=2)  )
  data_long$lgamma = -log10(data_long$gamma)
  data_long$ltrue = -log10(data_long$true)
  
  save(data_long, file="Exponential-Sim-Long.Rdata")
  
#########################################################################################  
  
#########################################################################################  
# ------------------------
#  Gamma
# ------------------------
  
  # function alpha as (mu,s) functions
  func.alpha <- function(mu,s) {mu^2/s^2}
  
  # function alpha as (mu,s) functions
  func.beta <- function(mu,s) {mu/s^2}
  
  
  #alpha
  alpha <- outer(mu, s,  Vectorize(func.alpha))
  # True value of alpha 
  alpha <- unique(sort(alpha[alpha>0]))
  
  #beta
  beta <- outer(mu, s,  Vectorize(func.beta))
  
  # True value of beta 
  beta <- unique(sort(beta[beta>0]))
  
  params <- expand.grid(n = n, a= alpha, b = beta)
  
  params$true <- with(params, {
  mu <- a/b
  s <- sqrt(a/b^2)
  true <- s/mu
  })


save(params, file="Gamma-params.Rdata")

L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
for(i in 1:NROW(params)){
  r <- as.numeric(unlist(params[i,]))
  ## replicates
  res <- parallel:::mclapply(1:B, function(i){
    x <- rgamma(n = r[1], shape = r[2], rate = r[3])
    cvs(x)
  }, mc.cores = 4)
  res <- do.call(rbind, res)
  L[i,] <- colMeans((res-r[4])^2, na.rm = TRUE)
  if(i %% 5 == 0) cat(i, " ")
  #  if(i == NROW(params)) cat("\n")
}
out <- cbind(params, L)


# Add the Gamma measures
out$CQV7=out$edith/out$old
out$CQV8=out$edith8/out$old
out$CVMAD=out$MADmedian/out$old
out$CQVMnAD=out$MnADmedian/out$old

save(out, file="Gamma-Sim-Wide.Rdata")

data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
data_long$n=as.factor(data_long$n)
# data_long$m=as.factor(data_long$m)
# data_long$p=as.factor(format(data_long$p, digits=2)  )
data_long$lgamma = -log10(data_long$gamma)
data_long$ltrue = -log10(data_long$true)

save(data_long, file="Gamma-Sim-Long.Rdata")
#########################################################################################  

#########################################################################################  
# ------------------------
#  Chi^2 case
# ------------------------

# function nu as mu and s function 
func.nu <- function(m,s) {2/(s/m)^2}

# lambda as function of mu and sigma
nu <- unique(sort(outer(mu, s, Vectorize(func.nu))))

params <- expand.grid(n = n, nu = nu)
params$true <- with(params, {
  mu <- nu
  s <- sqrt(2*nu)
  s/mu
})

save(params, file="Chisquare-params.Rdata")

L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
for(i in 1:NROW(params)){
  r <- as.numeric(unlist(params[i,]))
  ## replicates
  res <- parallel:::mclapply(1:B, function(i){
  x <- rchisq(n = r[1], df = r[2])
  cvs(x)
  }, mc.cores = 4)
  res <- do.call(rbind, res)
  L[i,] <- colMeans((res-r[3])^2, na.rm = TRUE)
  if(i %% 5 == 0) cat(i, " ")
  #  if(i == NROW(params)) cat("\n")
  }
out <- cbind(params, L)

# Add the Gamma measures
out$CQV7=out$edith/out$old
out$CQV8=out$edith8/out$old
out$CVMAD=out$MADmedian/out$old
out$CQVMnAD=out$MnADmedian/out$old

save(out, file="Chisquare-Sim-Wide.Rdata")

data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
data_long$n=as.factor(data_long$n)
# data_long$m=as.factor(data_long$m)
# data_long$p=as.factor(format(data_long$p, digits=2)  )
data_long$lgamma = -log10(data_long$gamma)
data_long$ltrue = -log10(data_long$true)

save(data_long, file="Chisquare-Sim-Long.Rdata")

#########################################################################################  
# ------------------------------
#     Ex-Gaussian  --- see Ueda's and Box-Cox's paper
# ------------------------------

# shape parameter
nu <- c(0.7, 7, 14)

# function alpha as (mu,s) functions
func.mus <- function(mu,v) {mu-v}

# function alpha as (mu,s) functions s^2 > v^2
func.ss <- function(s,v) {s^2-v^2}

#mus
mu.s <- outer(mu, nu,  Vectorize(func.mus))

# mus
mu.s<- unique(sort(mu.s)) #[alpha>0]))

#sigma
sigma.s <- outer(s, nu,  Vectorize(func.ss))

# True value of beta 
sigma.s <- unique(sort(sigma.s[sigma.s>0]))

sigma.s <- sqrt(sigma.s)


params$true <- with(params, {
  media <- mu + nu
  s <- sqrt(sigma^2 + nu^2)
  s/media
})

params <- params[(0 <= params$true) & (params$true <= 1),  ]  

save(params, file="ExGaussian-params.Rdata")

L <- matrix(NA_real_, nrow = NROW(params), ncol = 5)
colnames(L) <- c('old','edith','edith8', 'MADmedian','MnADmedian')
for(i in 1:NROW(params)){
  r <- as.numeric(unlist(params[i,]))
  ## replicates
  res <- parallel:::mclapply(1:B, function(i){
    x <- rexGAUS(n = r[1], mu = r[2], sigma = r[3], nu = r[4])
    cvs(x)
  }, mc.cores = 4)
  res <- do.call(rbind, res)
  L[i,] <- colMeans((res-r[5])^2, na.rm = TRUE)
  if(i %% 5 == 0) cat(i, " ")
  #  if(i == NROW(params)) cat("\n")
}
out <- cbind(params, L)

# Add the Gamma measures
out$CQV7=out$edith/out$old
out$CQV8=out$edith8/out$old
out$CVMAD=out$MADmedian/out$old
out$CQVMnAD=out$MnADmedian/out$old

save(out, file="ExGaussian-Sim-Wide.Rdata")

data_long <- gather(out, Estimator, gamma, CQV7:CQVMnAD, factor_key=TRUE)
data_long$n=as.factor(data_long$n)
# data_long$m=as.factor(data_long$m)
# data_long$p=as.factor(format(data_long$p, digits=2)  )
data_long$lgamma = -log10(data_long$gamma)
data_long$ltrue = -log10(data_long$true)

save(data_long, file="ExGaussian-Sim-Long.Rdata")

proc.time() - ptm

print(ptm)
######################################################################################### 
