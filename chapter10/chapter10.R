library(rethinking)

N <- 2000
G <- sample(1:2, size = N, replace = TRUE)

# habilities
u <- rbern(N, 0.1)

D <- rbern(N, ifelse(G==1, u*0.5, 0.8) ) + 1

accept_rate_u0 <- matrix(c(0.1, 0.1, 0.1, 0.3), nrow = 2)
accept_rate_u1 <- matrix(c(0.2, 0.3, 0.2, 0.5), nrow = 2)

p <- sapply(1:N, function(i) ifelse(u[i]==0, accept_rate_u0[D[i],G[i]], accept_rate_u1[D[i],G[i]]))

A <- rbern(N, p)

data_sim <- list(A = A, D=D, G=G)

m1 <- ulam(
    alist(
      A ~ bernoulli(p),
      logit(p) <- a[G],
      a[G] ~ normal(0, 1)
    ), data=data_sim, chains = 4, cores = 4)

m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G, D],
    matrix[G,D]:a ~ normal(0, 1)
  ), data=data_sim, chains = 4, cores =4)

post2 <- extract.samples(m2)
post2$fm_contrast_D1 <- post2$a[,1,1] - post2$a[,2,1]

post2$fm_contrast_D2 <- post2$a[,1,2] - post2$a[,2,2]

dens(post2$fm_contrast_D1, 
     lwd=4, col=2, xlab = "F-M contrast each department")

dens(post2$fm_contrast_D2, 
     lwd=4, col=3, xlab = "F-M contrast each department", add = TRUE)

data_sim$u  <- u

m3 <- ulam( 
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G, D] + buA * u,
    matrix[G,D]:a ~ normal(0, 1),
    buA ~ normal(0,1)
  ), data=data_sim,
  constraint=list(buA="lower=0"),
  chains=4, cores = 8
)

post3 <- extract.samples(m3)

post3$fm_constrast_D1 <- post3$a[,1,1] - post3$a[,2,1]

post3$fm_constrast_D2 <- post3$a[,1,2] - post3$a[,2,2]


dens(post3$fm_constrast_D1, 
     lwd=4, col=2, xlab = "F-M contrast each department")

dens(post3$fm_constrast_D2, 
     lwd=4, col=4, xlab = "F-M contrast each department", add = TRUE)


### Sensitivity analysis


datl <- data_sim
datl$D1 <- ifelse(data_sim$D == 1, 1, 0)
datl$N = N
datl$b = c(1,1)
datl$g = c(1,0)

mGDu <- ulam(
  alist (
    A ~ bernoulli(p),
    logit(p) <- a[G, D] + b[G]*u[i],
    matrix[G,D]:a ~ normal(0, 1),
    
    D1 ~ bernoulli(q),
    logit(q) <- delta[G] + g[G]*u[i],
    delta[G] ~ normal(0, 1),
    
    vector[N]:u ~ normal(0, 1)), 
  data = datl, chains=4, cores=8)


post4 <- extract.samples(mGDu)












