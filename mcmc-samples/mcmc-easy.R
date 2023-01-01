x <- seq(from = 0, to = 10, length=20)
y <- exp(-x)
plot(x,y)
lines(x,y)

target <- function(x){
  if (x < 0) {
    return(0)
  } else {
    return(exp(-x))
  }
}



x <- rep(0, 1000)
x[1] <- 3     # this is just a starting value, which I’ve set arbitrarily to 3
for (i in 2:1000) {
  currentx <- x[i - 1]
  proposedx <- currentx + rnorm(1, mean = 0, sd = 1)
  A <- target(proposedx) / target(currentx)
  if (runif(1) < A) {
    x[i] <- proposedx
  } else {
    x[i] <- currentx
  }
}

plot(x)
hist(x)


easyMCMC <- function(niter, startval, proposalsd) {
  x <- rep(0, niter)
  x[1] <- startval
  for (i in 2:niter) {
    currentx <- x[i - 1]
    proposedx <- rnorm(1, mean = currentx, sd = proposalsd)
    A <- target(proposedx) / target(currentx)
    if (runif(1) < A) {
      x[i] <- proposedx
    } else {
      x[i] <- currentx
    }
  }
  return(x)
}

z1 <- easyMCMC(100000, 2, 1)
z2 <- easyMCMC(100000, 2, 1)
z3 <- easyMCMC(100000, 2, 1)

plot(log(z1), type = "l")
lines(log(z2), col = 2)
lines(log(z3), col = 3)


par(mfcol = c(3, 1)) # rather odd command tells R to put 3 graphs on a single page
hist(z1)
hist(z2)
hist(z3)

