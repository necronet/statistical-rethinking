library(rstan)
library(rethinking)

N <- 1000

G <- sample(1:2, size = N, replace=TRUE)
D <- rbern(N, ifelse(G==1, 0.3, 0.8)) + 1

accept_rate <- matrix(c(0.1, 0.3, 0.1, 0.3), nrow = 2)

A <- rbern(N, accept_rate[D, G])


data_sim <- data.frame(A = A, G = G, D = D)

data_sim2 <- aggregate(A ~ G + D, data_sim, sum)
data_sim2$N <- aggregate(A ~ G + D, data_sim, length)$A


m2_bin <- ulam(
  alist(
      A ~ binomial(N, p),
      logit(p) <- a[G, D],
      matrix[G, D]:a ~ normal(0, 1)
  ), data = data_sim2, chains=4, cores=8
)


data(UCBadmit)
d <- UCBadmit
data <- list(
  A = d$admit,
  N = d$applications,
  G = ifelse(d$applicant.gender=="female", 1, 2),
  D = as.integer(d$dept)
)

mG <- ulam(
  alist(
    A ~ binomial(N, p),
    logit(p) <- a[G],
    a[G] ~ normal(0, 1)
  ), data=data, chains = 4, cores = 8
)


mGD <- ulam(
  alist(
    A ~ binomial(N, p),
    logit(p) <- a[G, D],
    matrix[G,D]:a ~ normal(0, 1)
  ), data=data, chains = 4, cores = 8
)



### Now simulating the effect perceived gender

total_apps <- sum(data$N)

apps_per_dept <- sapply(1:6, function(i) sum(data$N[data$D==i]) )

p_G1 <- link(mGD, data = list(
    D = rep(1:6, times = apps_per_dept),
    N = rep(1, total_apps),
    G = rep(1, total_apps)
))


p_G2 <- link(mGD, data = list(
  D = rep(1:6, times = apps_per_dept),
  N = rep(1, total_apps),
  G = rep(2, total_apps)
))


dens(p_G1 - p_G2, lwd=4, col=2, xlab = "effect of gender perception")







