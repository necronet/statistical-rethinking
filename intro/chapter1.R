p_grid <- seq( from = 0, to =1, length.out = 2000)

prior <- rep(1, 2000)

likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- (likelihood * prior) / sum((likelihood * prior))

plot(p_grid, posterior2, type ="b")
lines(p_grid, posterior, type ="b", col = "grey")