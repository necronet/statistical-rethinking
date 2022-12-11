### Conditional Manaties

library(rethinking)
data(rugged)

d <- rugged

str(d)

d$log_gdp <- log(d$rgdppc_2000)

plot(d$rugged, d$rgdppc_2000)


dd <- d[complete.cases(d$rgdppc_2000), ]

dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)

dd$rugged_std <- dd$rugged / max(dd$rugged)

plot_priors_with_model = function(model) {
  
  prior <- extract.prior(model)
  
  plot(NULL, xlim=c(0,1), ylim=c(0.5, 1.5), xlab="rudgeness", ylab="gdp log std")
  
  abline(h=min(dd$log_gdp_std), lty = 2)
  abline(h=max(dd$log_gdp_std), lty = 2)
  
  
  rudge_seq = seq(from=-0.1, to=1.1, length.out = 30)
  cid = rbern(30) + 1
  
  mu = link(model, post=prior, data=data.frame(rugged_std = rudge_seq, cid = cid))
  
  
  for(i in 1:50) {
    lines(rudge_seq, mu[i,], col=col.alpha("black", 0.3))
  }
}


m8.1 <- quap( 
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + b * (rugged_std - 0.215),
    a ~ dnorm(1, 1),
    b ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dd)


m8.2 <- quap( 
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + b * (rugged_std - 0.215),
    a ~ dnorm(1, 0.1),
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dd)


plot_priors_with_model(m8.1)
plot_priors_with_model(m8.2)


### Adding different alpha intercept per continent


dd$cid = ifelse(dd$cont_africa, 1, 2)


m8.3 <- quap( 
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dd)


compare(m8.1, m8.2, m8.3)



rudge_seq = seq(from=-0.1, to=1.1, length.out = 30)

mu.NotAfrica <- link(m8.3, data = data.frame(cid = 2, rugged_std = rudge_seq))
mu.Africa <- link(m8.3, data = data.frame(cid = 1, rugged_std = rudge_seq))


mu.NotAfrica_mu <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica_ci <- apply(mu.NotAfrica, 2, PI)

mu.Africa_mu <- apply(mu.Africa, 2, mean)
mu.Africa_ci <- apply(mu.Africa, 2, PI)


dev.off()
plot(NULL, xlim=c(0,1), ylim=c(0.5, 1.5), xlab="rudgeness", ylab="gdp log std")

points(dd$rugged_std[dd$cont_africa==1], dd$log_gdp_std[dd$cont_africa==1], col="blue")
points(dd$rugged_std[!dd$cont_africa], dd$log_gdp_std[!dd$cont_africa], col="black")

lines(rudge_seq, mu.NotAfrica_mu)
shade(mu.NotAfrica_ci, rudge_seq)

lines(rudge_seq, mu.Africa_mu)
shade(mu.Africa_ci, rudge_seq)







