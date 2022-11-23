library(rethinking)
data(Howell1)
d2 <- Howell1[Howell1$age > 18, ]

mu.list <- seq(from=150, to=160,length.out = 200)
sigma.list <- seq(from=1, to=20,length.out = 200)

post <- expand.grid(mu = mu.list, sigma = sigma.list)

post$LL <- sapply(1:nrow(post), function(i) sum(
  dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)
) )

post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)

post$prob <- exp(post$prod - max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

sample.rows <- sample(1:nrow(post), size = 1e4, replace=TRUE, prob=post$prob)

sample.mu <- post$mu[sample.rows]

sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, pch=15, col=col.alpha(rangi2,0.1))

dens(sample.mu)

dens(sample.sigma)

### Using quap approximation

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- quap(flist, data=d2)
precis(m4.1)


### Linear regression model

xbar <- mean(d2$weight)
m4.3 <- quap( alist(
    height ~ dnorm(mu, sigma),
    mu ~ a + b*(weight - xbar),
    a ~ dnorm(178, 20),
    b ~dlnorm(0,1),
    sigma ~dunif(0,50)
), data=d2)

precis(m4.3)


blue50 <- rgb(0, 0, 255, max = 255, alpha = 180, names = "blue50")
round(vcov(m4.3), 3)

plot(height~weight, data=d2, col=rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
plot(height~weight, data=d2, col=rangi2)


curve(a_map + b_map*(x-xbar), add=TRUE)
#curve_1 <- post[sample(1:nrow(post), 1),]
#curve(curve_1$a + curve_1$b*(x-xbar), add=TRUE, col = blue50)


mu_at_50 <- post$a + post$b*(50-xbar)
dens(mu_at_50)


mu <- link(m4.3)
str(mu)

weight.seq <- seq(from=20, to=75, by =1)
mu <- link(m4.3, data=data.frame(weight=weight.seq))

plot(height ~ weight, d2, type="n")
for(i in 1:100) {
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2,0.1))
}

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)

plot(height ~ weight, data=d2, col=col.alpha(rangi2,0.5))
lines(weight.seq, mu.mean)
curve(a_map + b_map*(x-xbar), col="red", add=TRUE)
shade(mu.PI, weight.seq)


### Curves from lines

d <- Howell1
plot(height ~ weight, d)

# Standardize a parameter
d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2

m4.5 <- quap (
    alist( height~ dnorm(mu, sigma),
           mu <- a + b1*weight_s + b2*weight_s2,
           a ~ dnorm(178,20),
           b1 ~ dlnorm(0, 1),
           b2 ~dnorm(0, 1),
           sigma ~ dunif(0,50)
    ), data=d
)

precis(m4.5)

weight.seq <- seq(from=-2.2, to =2.2, length.out=34)

pred_dat <- list(weight_s=weight.seq, weight_s2=weight.seq^2)

mu <- link(m4.5, data=pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.5, data=pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

red_desat <- col.desat("red", 0.3)

dev.off()
plot(height~weight_s, d, col=col.alpha(rangi2, 0.3))

for(i in 1:100) {
  points(weight.seq, mu[i,], pch=16, col=col.alpha(red_desat,0.1))
}
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

## Cubic model
d$weight_s3 <- d$weight_s^3
m4.6 <- quap ( 
  alist( height~ dnorm(mu, sigma),
         mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3,
         a ~ dnorm(178,20),
         b1 ~ dlnorm(0, 1),
         b2 ~dnorm(0, 1),
         b3 ~dnorm(0, 1),
         sigma ~ dunif(0,50)
  ), data=d
)







