library(rethinking)
library(igraph)


N <- 25
dyads <- t(combn(N, 2))
N_dyads <- nrow(dyads)

# Simulate friendships

f <- rbern(N_dyads, 0.1)
alpha <- (-3)
y <- matrix(NA, N, N)

for ( i in 1:N ) {
  for ( j in 1:N ) {
    if ( i != j ) {
      # directed tie from i to j
      ids <- sort( c(i,j) )
      the_dyad <- which( dyads[,1]==ids[1] & dyads[,2]==ids[2] )
      p_tie <- f[the_dyad] + (1-f[the_dyad])*inv_logit( alpha )
      y[i,j] <- rbern( 1 , p_tie )
    }
  }#ij
}

giftsAB <- rep(NA, N_dyads)
giftsBA <- rep(NA, N_dyads)

lambda <- log(c(0.5, 2))

for(i in 1:N_dyads) {
  A <- dyads[i, 1]
  B <- dyads[i, 2]
  giftsAB[i] <- rpois(1, exp(lambda[1+y[A,B]]))
  giftsBA[i] <- rpois(1, exp(lambda[1+y[A,B]]))
}


# draw network
sng <- graph_from_adjacency_matrix(y)
lx <- layout_nicely(sng)
vcol <- "#DE536B"
plot(sng , layout=lx , vertex.size=8 , edge.arrow.size=0.75 , edge.width=2 , edge.curved=0.35 , vertex.color=vcol , edge.color=grau() , asp=0.9 , margin = -0.05 , vertex.label=NA )


# analyze synthetic data
sim_data <- list(
  N_dyads = N_dyads,
  N_households = N,
  D = 1:N_dyads,
  HA = dyads[,1],
  HB = dyads[,2],
  GAB = giftsAB,
  GBA = giftsBA )



# dyad model
f_dyad <- alist(
  GAB ~ poisson( lambdaAB ),
  GBA ~ poisson( lambdaBA ),
  log(lambdaAB) <- a + T[D,1] ,
  log(lambdaBA) <- a + T[D,2] ,
  a ~ normal(0,1),
  
  ## dyad effects - non-centered
  transpars> matrix[N_dyads,2]:T <-
    compose_noncentered( rep_vector(sigma_T,2) , L_Rho_T , Z ),
  matrix[2,N_dyads]:Z ~ normal( 0 , 1 ),
  cholesky_factor_corr[2]:L_Rho_T ~ lkj_corr_cholesky( 2 ),
  sigma_T ~ exponential(1),
  
  ## compute correlation matrix for dyads
  gq> matrix[2,2]:Rho_T <<- Chol_to_Corr( L_Rho_T )
)

mGD <- ulam( f_dyad , data=sim_data , chains=4 , cores=4 , iter=2000 )
trankplot(mGD)
precis( mGD , depth=3 , pars=c("a","Rho_T","sigma_T") )

post <- extract.samples(mGD)
T_est <- apply(post$T,2:3,mean)
# convert to adjacency matrix
y_est <- y
for ( i in 1:N_dyads ) {
  y_est[ dyads[i,1] , dyads[i,2] ] <- T_est[i,1]
  y_est[ dyads[i,2] , dyads[i,1] ] <- T_est[i,2]
}#i

# show discrimination as densities for each true tie state
dens( y_est[y==0] , xlim=c(-0.5,1.5) , lwd=4 , col=2 , xlab="posterior mean T" )
dens( y_est[y==1] , add=TRUE , lwd=4 , col=4 )

# show correlation by true friend state
plot( T_est[,1] , T_est[,2] , lwd=3 , col=ifelse(f==1,6,1) , xlab="Household A" , ylab="Household B" )

# show reciprocity
dens( post$Rho_T[,1,2] , lwd=4 , col=2 , xlab="correlation within dyads" , xlim=c(-1,1) )


data(KosterLeckie)

# analyze sample
kl_data <- list(
  N_dyads = nrow(kl_dyads),
  N_households = max(kl_dyads$hidB),
  D = 1:nrow(kl_dyads),
  HA = kl_dyads$hidA,
  HB = kl_dyads$hidB,
  GAB = kl_dyads$giftsAB,
  GBA = kl_dyads$giftsBA )

mGDkl <- ulam( f_dyad , data=kl_data , chains=4 , cores=4 , iter=4000 )

precis( mGDkl , depth=3 , pars=c("a","Rho_T","sigma_T") )

post <- extract.samples(mGDkl)
dens( post$Rho_T[,1,2] , lwd=4 , col=2 , xlab="correlation within dyads" , xlim=c(-1,1) )
