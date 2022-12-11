library(rethinking)

data("Laffer")

d <- Laffer

d$Rate <- standardize(d$tax_rate)
d$Revenue <- standardize(d$tax_revenue)

m7H1a <- quap(
  alist(
    Revenue ~ dnorm(mu, sigma),
    mu <- a + b * Rate,
    a ~ dnorm(0, 0.2),
    b ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

m7H1b <- quap(
  alist(
    Revenue ~ dnorm(mu, sigma),
    mu <- a + b * Rate + b2 * Rate^2,
    a ~ dnorm(0, 0.2),
    c(b, b2) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)


# cubic model
m7H1c <- quap(
  alist(
    Revenue ~ dnorm(mu, sigma),
    mu <- a + b * Rate + b2 * Rate^2 + b3 * Rate^3,
    a ~ dnorm(0, 0.2),
    c(b, b2, b3) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)


comparison <- compare(m7H1a, m7H1b, m7H1c)
comparison


## base sequence for predictions
plot_df <- data.frame(Rate = seq(from = min(d$Rate), to = max(d$Rate), length.out = 1e4))
## Predictions for Linear Model
plot_df$m7H1a <- apply(link(m7H1a, data = plot_df), 2, mean)
plot_df$m7H1aLower <- apply(link(m7H1a, data = plot_df), 2, PI, prob = .999999)[1, ]
plot_df$m7H1aUpper <- apply(link(m7H1a, data = plot_df), 2, PI, prob = .999999)[2, ]
## Predictions for Quadratic Model
plot_df$m7H1b <- apply(link(m7H1b, data = plot_df), 2, mean)
plot_df$m7H1bLower <- apply(link(m7H1b, data = plot_df), 2, PI, prob = .999999)[1, ]
plot_df$m7H1bUpper <- apply(link(m7H1b, data = plot_df), 2, PI, prob = .999999)[2, ]
## Predictions for Cubic Model
plot_df$m7H1c <- apply(link(m7H1c, data = plot_df), 2, mean)
plot_df$m7H1cLower <- apply(link(m7H1c, data = plot_df), 2, PI, prob = .999999)[1, ]
plot_df$m7H1cUpper <- apply(link(m7H1c, data = plot_df), 2, PI, prob = .999999)[2, ]
## Plotting
ggplot(plot_df) +
  geom_point(data = d, aes(x = Rate, y = Revenue), size = 2) +
  geom_line(data = plot_df, aes(y = m7H1a, x = Rate, colour = "Linear"), size = 1.5) +
  geom_ribbon(data = plot_df, aes(ymin = m7H1aLower, ymax = m7H1aUpper, x = Rate), alpha = .1) +
  geom_line(data = plot_df, aes(y = m7H1b, x = Rate, colour = "Quadratic"), size = 1.5) +
  geom_ribbon(data = plot_df, aes(ymin = m7H1bLower, ymax = m7H1bUpper, x = Rate), alpha = .1) +
  geom_line(data = plot_df, aes(y = m7H1c, x = Rate, colour = "Cubic"), size = 1.5) +
  geom_ribbon(data = plot_df, aes(ymin = m7H1cLower, ymax = m7H1cUpper, x = Rate), alpha = .1) +
  theme_bw() +
  scale_colour_discrete(name = "Model")





