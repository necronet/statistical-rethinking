#### Chapter 7 Ulysses Compass




### Entropy and KL Divergence
#qs <- cbind(c(0.25, 0.3), c(0.75, 0.7))
p <- c(0.3, 0.7 ) 
N = 1000
qs <- cbind( seq(0, 1, length.out = N), seq(1, 0, length.out = N) )
kldivergence <- function(q1, q2) {p <- c(0.3, 0.7 ); sum( p*log(p/c(q1,q2)))}
kldivergence_results <- mapply(kldivergence, qs[,1], qs[,2])
plot(qs[,1], kldivergence_results, pch=20, col='blue')




### Loading data brain size and body size
sppnames <- c( "afarensis","africanus","habilis","boisei", "rudolfensis","ergaster","sapiens") 
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 ) 
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 ) 
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

d$mass_std <- (d$mass - mean(d$mass))/sd(d$mass) 
d$brain_std <- d$brain / max(d$brain)

m7.1 <- quap(
  alist( brain_std ~ dnorm( mu , exp(log_sigma) ), 
         mu <- a + b*mass_std, 
         a ~ dnorm( 0.5 , 1 ), 
         b ~ dnorm( 0 , 10 ), 
         log_sigma ~ dnorm( 0 , 1 )
  ), data=d )

precis(m7.1)
lppd( m7.1 , n=1e4 )


logprob <- sim( m7.1 , ll=TRUE , n=1e4 )
n <- ncol(logprob) 
ns <- nrow(logprob)

f <- function( i ) log_sum_exp( logprob[,i] ) - log(ns)
( lppd <- sapply( 1:n , f ) )



### Simulate training sample
N <- 20 
kseq <- 1:5 

dev <- sapply( kseq , function(k) {
    print(k);
    
    r <- mcreplicate( 
                  1e4,
                  sim_train_test( N=N, k=k ), mc.cores=10); 
    
    c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) ) 
  }
)



plot(
  1:5,
  dev[1,],
  ylim = c(min(dev[1:2,]) - 5 , max(dev[1:2,]) + 10),
  xlim = c(1, 5.1),
  xlab = "number of parameters",
  ylab = "deviance",
  pch = 16 ,
  col = rangi2
)
  
mtext(concat("N = ", N))
points((1:5) + 0.1 , dev[2, ])

for (i in kseq) {
  pts_in <- dev[1, i] + c(-1, +1) * dev[3, i]
  pts_out <- dev[2, i] + c(-1, +1) * dev[4, i]
  lines(c(i, i) , pts_in , col = rangi2)
  lines(c(i, i) + 0.1 , pts_out)
}



