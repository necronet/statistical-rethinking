### Chapter 5 exercise

set.seed(42)
N <- 1e2
Elev <- rnorm(n = N, mean = 0, sd = 1)
VegHeight <- rnorm(n = 100, mean = -Elev, sd = 1)
AirTemp <- rnorm(n = N, mean = Elev, sd = 2)
d <- data.frame(Elev, VegHeight, AirTemp)



# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  d$Color[d$AirTemp > median(d$AirTemp)] = "blue"
  d$Color[d$AirTemp < median(d$AirTemp)] = "red"
  
  points(x,y, pch = 19, col = d$Color)
}
# Create the plots
pairs(d[,1:3], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


m <- quap(
  alist(
    VegHeight ~ dnorm(mu, sigma),
    mu <- a + bAT * AirTemp,
    a ~ dnorm(0, 1),
    bAT ~ dnorm(0, 1),
    sigma ~ dunif(0, 2)
  ),
  data = d
)
precis(m)


m2 <- quap(
  alist(
    VegHeight ~ dnorm(mu, sigma),
    mu <- a + bAT * AirTemp + bEL * Elev,
    a ~ dnorm(0, 1),
    bAT ~ dnorm(0, 1),
    bEL ~ dnorm(0, 1),
    sigma ~ dunif(0, 2)
  ),
  data = d
)
precis(m2)


m3 <- quap(
  alist(
    VegHeight ~ dnorm(mu, sigma),
    mu <- a + bEL * Elev,
    a ~ dnorm(0, 1),
    bEL ~ dnorm(0, 1),
    sigma ~ dunif(0, 2)
  ),
  data = d
)
precis(m3)


### Simulation #2

N <- 1e2
rho <- 0.6
L <- rnorm(n = N, mean = 0, sd = 1)
H <- rnorm(n = N, mean = rho * L, sd = sqrt(1 - rho^2))
S <- rnorm(n = N, mean = L - H, sd = 1)
d <- data.frame(S, L, H)

m2 <- quap(
  alist(
    S ~ dnorm(mu, sigma),
    mu <- a + bL * L,
    a ~ dnorm(0, 1),
    bL ~ dnorm(0, 1),
    sigma ~ dunif(0, 2)
  ),
  data = d
)
precis(m2)


m3 <- quap(
  alist(
    S ~ dnorm(mu, sigma),
    mu <- a + bH * H + bL * L,
    a ~ dnorm(0, 1),
    bH ~ dnorm(0, 1),
    bL ~ dnorm(0, 1),
    sigma ~ dunif(0, 2)
  ),
  data = d
)
precis(m3)



data("foxes")

d <- foxes
ma <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area,
    a ~ dnorm(5, 5),
    ba ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(ma)

area.seq <- seq(from = min(d$area), to = max(d$area), length.out = 1e4)

mu <- link(ma, data = data.frame(area = area.seq))

mu.PI <- apply(mu, 2, PI, prob = 0.95)

plot(weight ~ area, data = d, col = rangi2)
abline(ma)

shade(mu.PI, area.seq)

### Working with groupsize now

mg <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bg * groupsize,
    a ~ dnorm(5, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(mg)

groupsize.seq <- seq(from = min(d$groupsize), to = max(d$groupsize), length.out = 1e4)
mu <- link(mg, data = data.frame(groupsize = groupsize.seq))
mu.PI <- apply(mu, 2, PI, prob = 0.95)
plot(weight ~ groupsize, data = d, col = rangi2)
abline(mg)
shade(mu.PI, groupsize.seq)



### Working with both association

mag <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area + bg * groupsize,
    a ~ dnorm(5, 5),
    ba ~ dnorm(0, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(mag)

pairs(d[,3:5])




G.avg <- mean(d$groupsize)
A.seq <- seq(from = 0, to = 6, length.out = 1e4)
pred.data <- data.frame(
  groupsize = G.avg,
  area = A.seq
)

red_desat <- col.desat("red", 0.3)

mu <- link(mag, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
A.sim <- sim(mag, data = pred.data, n = 1e4)
A.PI <- apply(A.sim, 2, PI)
A.mu <- apply(A.sim, 2, mean)

dev.off()
plot(weight ~ area, data = d, type = "n")
mtext("groupsize = 4.345")
lines(A.seq, A.mu, col=col.alpha("red",0.4))
lines(A.seq, mu.mean)
#points(A.sim, pch=19, col=col.alpha(red_desat,0.1))
points(d$area, d$weight, pch=20, col=col.alpha("blue",0.5))
shade(mu.PI, A.seq)
shade(A.PI, A.seq)





