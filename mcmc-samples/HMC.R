# HMC as explained by statistical rethinking

N <- 500

# These are independently sample random variables
y <- rnorm(N, mean = 0, sd = 1)
x <- rnorm(N, mean = 0, sd = 1)

# Scaling & Centering the random variables
# This might be optional considering the random sample is already center at 0 and with a sd=1
y <- as.numeric(scale(y))
x <- as.numeric(scale(x))

U <- function(q,
              a = 0,
              b = 1,
              k = 0,
              d = 1) {
  
  muy = q[1]
  mux = q[2]
  
  U <- sum (
      dnorm(y, muy, 1, log = TRUE) + dnorm(x, mux, 1, log = TRUE) +
      dnorm(muy, a, b, log = TRUE) + dnorm(mux, k, d, log = TRUE)
  )
  
  return(-U)
}

U_gradient <- function(q,
                          a = 0,
                          b = 1,
                          k = 0,
                          d = 1) {
  muy = q[1]
  mux = q[2]
  
  G1 <- sum(y - muy) + (a-muy)/b^2
  G2 <- sum(x - mux) + (k-mux)/d^2
  
  return(c(-G1, -G2))
}


library(shape)
Q <- list()

Q$q <- c(-0.1,0.2)

pr <- 0.3
plot( NULL , ylab="muy" , xlab="mux" , xlim=c(-pr,pr) , ylim=c(-pr,pr) )
step <- 0.02


L <- 15 # 0.03/28 for U-turns --- 11 for working example 

n_samples <- 10000
path_col <- col.alpha("black",0.5)
points( Q$q[1] , Q$q[2] , pch=4 , col="black" )



for (i in 1:n_samples) {
  Q <- HMC2(U , U_gradient , step , L , Q$q)
  if (n_samples < 6000) {
    for (j in 1:L) {
      K0 <- sum(Q$ptraj[j, ] ^ 2) / 2 # kinetic energy
      #lines(Q$traj[j:(j + 1), 1] , Q$traj[j:(j + 1), 2] , col = path_col , lwd = 1 + 2 * K0)
    }
    
    points(Q$traj[1:L + 1, ] ,pch = 16 , col = "white" , cex = 0.35)
    
    #Arrows(Q$traj[L, 1] , Q$traj[L, 2] , Q$traj[L + 1, 1] , Q$traj[L + 1, 2] , arr.length = 0.35 , arr.adj = 0.7)
    #text(Q$traj[L + 1, 1] , Q$traj[L + 1, 2] , i , cex = 0.8 , pos = 4 , offset = 0.4)
  }
  points(Q$traj[L + 1, 1] , Q$traj[L + 1, 2] , pch = ifelse(Q$accept == 1 , 16 , 1) , col = ifelse(abs(Q$dH) > 0.2 , col.desat("red", 0.5) , "black"))
  
  
}
