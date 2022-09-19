library(rethinking)
data(homeworkch3)



total_boys <- sum(birth1) + sum(birth2)
total_observations <- length(birth1) + length(birth2)

p_grid <- seq(0, 1, length.out = 1000)
prior <- rep(1, length(p_grid))
likelihood <- dbinom(total_boys, size = total_observations, prob = p_grid)

posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid, posterior, type = "l")

p_grid[which.max(posterior)]

trials <- 10000

p_samples <- sample(p_grid, size = trials, prob = posterior, replace=TRUE)
plot(p_samples)

(hpi_50 <- HPDI(p_samples, .5))
(hpi_89 <- HPDI(p_samples, .89))
(hpi_97 <- HPDI(p_samples, .97))

n_trials <- 10000
b_sample <- rbinom(n_trials, size = total_observations, prob = p_samples)

hist(b_sample)
dens(b_sample)

simplehist(b_sample)



