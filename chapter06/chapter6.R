### The Causual Terror - The haunted DAG


###################################################################################################
### Simulating Berkon's Paradox ###################################################################
###################################################################################################

N <- 200
p <- 0.1

nw <- rnorm(N)
tw <- rnorm(N)

s <- nw + tw
q <- quantile(s, 1-p)

selected <- ifelse(s >= q, TRUE, FALSE)
cor(tw[selected], nw[selected])


### Graphicall would look like this
dev.off()
plot(nw, tw)
points(tw[selected], nw[selected], col='red')

###################################################################################################
##########################################END OF SECTION###########################################
###################################################################################################



###################################################################################################
################ Multicollinear legs
###################################################################################################

N - 100

height <- rnorm(N, 10, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop*height + rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + rnorm( N , 0 , 0.02 )

data <-  data.frame(height, leg_left, leg_right)

m6.1 <- quap(
  alist( 
      height ~ dnorm(mu, sigma),
      mu <- a + bl*leg_left + br*leg_right,
      a ~ dnorm(10, 100),
      bl ~ dnorm(2, 10),
      br ~ dnorm(2, 10),
      sigma ~ dexp(1)),
  data= data
)

post <- extract.samples(m6.1) 
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

sum_blbr <- post$bl + post$br 

dens( sum_blbr , col=rangi2 , lwd=2 , xlab="sum of bl and br" )



### running only with one leg

m6.2 <- quap(
  alist( 
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    sigma ~ dexp(1)),
  data= data
)

precis(m6.2)


#### Multicollinear milk

data(milk) 
d <- milk 
d$K <- scale( d$kcal.per.g ) 
d$F <- scale( d$perc.fat ) 
d$L <- scale( d$perc.lactose )

m6.3 <- quap(
  alist( 
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF*F , 
    a ~ dnorm( 0 , 0.2 ) , 
    bF ~ dnorm( 0 , 0.5 ) , 
    sigma ~ dexp( 1 )
  ) , data=d )


m6.4 <- quap(
  alist( 
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bL*L , 
    a ~ dnorm( 0 , 0.2 ) , 
    bL ~ dnorm( 0 , 0.5 ) , 
    sigma ~ dexp( 1 )
  ) , data=d )

precis(m6.3)
precis(m6.4)

m6.5 <- quap(
  alist( K ~ dnorm( mu , sigma ) ,
         mu <- a + bF*F + bL*L , 
         a ~ dnorm( 0 , 0.2 ) , 
         bF ~ dnorm( 0 , 0.5 ) , 
         bL ~ dnorm( 0 , 0.5 ) , 
         sigma ~ dexp( 1 )
  ) , data=d ) 

precis( m6.5 )

pairs( ~ kcal.per.g + perc.fat + perc.lactose , data=d , col=rangi2 )


#### Overthinking: Simulating collinearity.

d <- milk 
sim.coll <- function( r=0.9 ) { 
  d$x <- rnorm(nrow(d), mean=r*d$perc.fat, sd=sqrt( (1-r^2)*var(d$perc.fat) ) )
  m <- lm( kcal.per.g ~ perc.fat + x , data=d )
  sqrt( diag( vcov(m) ) )[2] # stddev of parameter
}

rep.sim.coll <- function( r=0.9 , n=100 ) {
  stddev <- replicate( n , sim.coll(r) ) 
  mean(stddev)
}
r.seq <- seq(from=0,to=0.99,by=0.01) 
stddev <- sapply( r.seq , function(z) rep.sim.coll(r=z,n=100))
plot( stddev ~ r.seq , type="l" , col=rangi2, lwd=2 , xlab="correlation" )

###################################################################################################
##########################################END OF SECTION###########################################
###################################################################################################



###################################################################################################
################ Post-treatment bias
###################################################################################################


N <- 100
h0 <- rnorm(N, 10, 2)
treatment <- rep(0:1, each=N/2)
fungus <- rbinom(N, size=1, prob=0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, 5 - 3*fungus)

d <- data.frame(h0=h0, h1 = h1, treatment=treatment, fungus = fungus)

precis(d)



### Building priors
m6.7 <- quap(
  alist( h1 ~ dnorm( mu , sigma ), 
         mu <- h0 * p, 
         p <- a + bt*treatment + bf*fungus, 
         a ~ dlnorm( 0 , 0.2 ) , 
         bt ~ dnorm( 0 , 0.5 ), 
         bf ~ dnorm( 0 , 0.5 ), 
         sigma ~ dexp( 1 )
  ), data=d )

precis(m6.7)


### Let's try to ignore the treatment variable fungus

m6.8 <- quap(
  alist( h1 ~ dnorm( mu , sigma ), 
         mu <- h0 * p, 
         p <- a + bt*treatment, 
         a ~ dlnorm( 0 , 0.2 ), 
         bt ~ dnorm( 0 , 0.5 ),
         sigma ~ dexp( 1 )
  ), data=d )

precis(m6.8)


###################################################################################################
################ Colliders Bias
###################################################################################################


library(rethinking) 
d <- sim_happiness( seed=1977 , N_years=1000 ) 
precis(d)


plot(d$age, d$happiness)
points(d$age[d$married == 1], d$happiness[d$married == 1], col='blue', pch = 19)


d2 <- d[ d$age>17 , ] # only adults 
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

d2$mid <- d2$married + 1

m6.9 <- quap(
  alist( happiness ~ dnorm( mu , sigma ), 
         mu <- a[mid] + bA*A, 
         a[mid] ~ dnorm( 0 , 1 ),
         bA ~ dnorm( 0 , 2 ),
         sigma ~ dexp(1)
  ) , data=d2 )

precis(m6.9,depth=2)

### omitting the marriage index variable
m6.10 <- quap(
  alist( happiness ~ dnorm( mu , sigma ), 
         mu <- a + bA*A, a ~ dnorm( 0 , 1 ), 
         bA ~ dnorm( 0 , 2 ), 
         sigma ~ dexp(1)
  ) , data=d2 )

precis(m6.10)





  