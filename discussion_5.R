library(tidyverse)
library(ggplot2)

n <- 10
p <- 0.1
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

# Generate data
x <- 0:n
binomial_probs <- dbinom(x, size = n, prob = p) 
normal_probs <- dnorm(x, mean = mu, sd = sigma)

# Convert to data frame to plot
df <- data.frame(x = x, 
                 Binomial = binomial_probs, 
                 Normal = normal_probs)

# Plot
df %>% 
ggplot(aes(x = x)) +
  geom_bar(aes(y = Binomial), stat = "identity", fill = "gray", alpha = 0.5) + 
  geom_line(aes(y = Normal), color = "blue") +
  labs(title = "Binomial vs. Normal Distributions: n = 10, p = .1",
       y = "Probability") +
  theme_minimal()

