library(tidyverse)
library(ggplot2)
library(scales)

#---------------------
# Problem 1 - Linear Regression
#---------------------

# Load data
data("cars")
glimpse(cars)

# 1. Data Visualization
# Stopping distance increases with speed in a roughly linear pattern, with more variability at higher speeds and possibly a couple outliers.

plt1 <- cars %>% 
ggplot(aes(x = speed, y = dist)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "darkred") +
  labs(title = "Stopping Distance vs Speed",
       x = "Speed (mph)", y = "Stopping Distance (ft)")

plt1

# 2. Linear Model
# Stopping Distance = 3.932409 * speed - 17.58

# Model
mod_dist <- lm(dist ~ speed, data = cars)

# Summarize 
summary(mod_dist)

# Add predicted values (don't have to replot, same as above) 
df_cars_model <- cars %>% 
  mutate(predicted_dist = predict(mod_dist))

glimpse(df_cars_model)

# 3. Evaluate R-Squared and Residuals

# A. According to the above summary, R-squared is 0.6511 so ~65% of stopping distance is explained by speed.

# B. Residuals

# Add to dataframe
df_cars_model <- df_cars_model %>% 
  mutate(
    .fitted = fitted(mod_dist),
    .resid = resid(mod_dist),
    .std_resid = rstandard(mod_dist)
  )

glimpse(df_cars_model)

## Diagnostic Plots
# Residual tests show a reasonable fit with roughly normal distribution of residuals and some heteroscedasticity.

### 1. Residuals vs Fitted
#- Checks: Linearity and equal variance
#- Good: Random scatter around zero
#- Bad: Curve = nonlinearity, Fan = heteroscedasticity

# Slight fan shape suggests mild heteroscedasticity with increasing variance at higher speeds

plot_resid <- df_cars_model %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

plot_resid

# Histogram of Residuals
# Broadly but not totally normally distributed with a slight right skew

plot_resid_hist <- df_cars_model %>% 
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal()

plot_resid_hist

### Q-Q
#- Checks: Normality of residuals
#- Good: Points fall on line
#- Bad: Curved tails = non-normality or outliers

# Follows the line generally with deviation at both ends: suggests non-normality

plot_qq <- df_cars_model %>% 
  ggplot(aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

plot_qq

# shapiro-wilkes test
# p<0.05 suggests non-normality (null hypothesis = data is normal, which is rejected)

df_cars_model$.resid %>% 
  shapiro.test()






plot(mod1$fitted.values, mod1$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")


# Conclusion

# Based on the model summary and residual analysis, the linear model is acceptable. The R-squared value is meaningful but not strong. Linearity, normality, and constant variance are mostly met, with slight heteroscedasticity and mild non-normality in residuals. Independence was not formally tested, but it's reasonable to assume since each car's stopping distance is unrelated to others. Model fit might improve by adding more predictors.




# 2 WHO dataset

df_who_raw <- read_csv("https://raw.githubusercontent.com/AmandaSFox/DATA605_Math/main/Assignment_3/who.csv")
glimpse(df_who_raw)

# Clean data

# check NA
df_who_raw %>% 
  summarise(across(everything(), ~sum(is.na(.))))

# check duplicate rows
nrow(df_who_raw) - nrow(distinct(df_who_raw))

# check unique values in each column
df_who_raw %>% 
  summarise(across(everything(), n_distinct))

# check col of NA values 
unique(df_who_raw$...10)

# Check if both LifeExp columns are identical
all(df_who_raw$`LifeExp...2` == df_who_raw$`LifeExp...12`)

# drop second LifeExp and empty column, rename LifeExp
df_who_clean <- df_who_raw %>% 
  select(- LifeExp...12,- ...10) %>%
  rename(LifeExp = `LifeExp...2`)

glimpse(df_who_clean)

# summarize and plot
summary(df_who_clean)

plt3 <- df_who_clean %>% 
  ggplot(aes(x = TotExp, y = LifeExp)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_x_continuous(labels = scales::comma)+
    labs(
    title = "Life Expectancy vs Total Health Expenditures",
    x = "Total Health Expenditures",
    y = "Life Expectancy"
  ) +
  theme_minimal()

plt3


mod_life <- lm(LifeExp ~ TotExp, data = df_who_clean)

summary(mod_life)

df_who_model <- df_who_clean %>% 
  mutate(predicted_life = predict(mod_life),
         .fitted = fitted(mod_life),
         .resid = resid(mod_life),
         .std_resid = rstandard(mod_life))

glimpse(df_who_model)


plot_resid_who <- df_who_model %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

plot_resid_who

plot_hist_who <- df_who_model %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal()

plot_hist_who

plot_qq_who <- df_who_model %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

plot_qq_who

shapiro.test(df_who_model$.resid)


