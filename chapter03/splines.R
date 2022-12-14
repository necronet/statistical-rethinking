### Splines section

### Cherry blossoms

library(rethinking)
library(splines)

data(cherry_blossoms)

d <- cherry_blossoms

precis(d)

plot(doy~year, data = d, col=rangi2)

d2 <- d[complete.cases(d$doy), ]
num_knots <- 30
knot_list <- quantile(d2$year, prob=seq(0, 1, length.out=num_knots))

B <- bs(d2$year, knots = knot_list[-c(1,num_knots)], degree = 3, intercept = TRUE)

plot(NULL, xlim = range(d2$year), ylim=c(0,1), xlab = "Year", ylab = "basis")

for(i in 1:ncol(B)) {
  lines(d2$year, B[,i])
}
  
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100,10),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data = list( D=d2$doy, B = B ), start = list(w = rep(0, ncol(B)))
)

precis(m4.7)

post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d$year), ylim=c(-6,6), xlab="Year", ylab="basis * weight")
for(i in 1:ncol(B)) {
  lines(d2$year, w[i]*B[,i])
}

mu <- link(m4.7)
mu_PI <- apply(mu, 2, PI, 0.97)

plot(d2$year, d2$doy, col=col.alpha(rangi2, 0.3), pch = 16)
shade(mu_PI, d2$year, col=col.alpha("black", 0.5))


