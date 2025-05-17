

library(tidyverse)
library(ggplot2)

# Load data
data("cars")

# 1. Data Visualization

plt1 <- cars %>% 
ggplot(aes(x = speed, 
                 y = dist)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "darkred") +
  labs(title = "Stopping Distance vs Speed",
       x = "Speed (mph)", y = "Stopping Distance (ft)")

plt1

# 2. Linear Model

mod1 <- cars %>% 
  lm(dist ~ speed, 
     data = .)

summary(mod1)

# 3. Evaluate Model

# R-squared is 0.6511 so ~65% of stopping distance is explained by speed.


