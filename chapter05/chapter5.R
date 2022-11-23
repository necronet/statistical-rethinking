### Chapter 5  Many variables and spurious waffle
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)


# Explaining the fork confound variables

n <- 1000

Z <- rbern(n, 0.5)
X <- rbern(n, (1-Z)*0.1 + 0.9*Z)
Y <- rbern(n, (1-Z)*0.1 + 0.9*Z)

table(X,Y)
cor(X, Y)


table(X[Z==0], Y[Z==0])

cor(X[Z==0], Y[Z==0])

table(X[Z==1], Y[Z==1])
cor(X[Z==1], Y[Z==1])


### Multiple regression 

m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M+ bA *A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(m5.3)





