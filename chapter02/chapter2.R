## Chapter 2 
library(rethinking)
library(ggplot2)
library(cowplot)
library(stringr)

data1 <- c("W", "W", "W")
data2 <- c("W", "W", "W", "L")
data3 <- c("L", "W", "W", "L", "W", "W", "W")

data <- list(data1, data2, data3)


p_grid <- seq(0, 1, length.out = 25)
prior <- rep(1, length(p_grid))
prior_non_uniform <- ifelse(p_grid < 0.5, 0, 1)


par(mfrow=c(3,2))


for (i in 1:length(data)) {
  d <- data[[i]]
  observations <- length(d)
  w <- sum(d == "W")
  
  title <- "Posterior with uniform prior"
  title <- paste0(title, paste(d, collapse = " "))
  likelihood <- dbinom(w, size = observations, prob = p_grid)
  posterior <- likelihood * prior
  posterior <- posterior / sum(posterior)
  plot(p_grid, posterior, type ="b")  
  title(title)
  
  ## Use the non uniform prior
  title <- "Posterior with non uniform prior"
  title <- paste0(title, paste(d, collapse = " "))
  likelihood <- dbinom(w, size = observations, prob = p_grid)
  posterior <- likelihood * prior_non_uniform
  posterior <- posterior / sum(posterior)
  plot(p_grid, posterior, type ="b")  
  title(title)
  
}


sample(p_grid, prob=posterior, size = 10)

fake_posterior <- rep(0,25)
sample(p_grid, prob=fake_posterior, size = 20, replace = TRUE)


