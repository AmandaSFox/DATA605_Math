library(tidyverse)
library(ggplot2)
library(purrr)
library(summarytools)

#-------------------------------
# EDA
#-------------------------------

# load data
df_raw <- read_csv("https://raw.githubusercontent.com/AmandaSFox/DATA605_Math/refs/heads/main/gapminder_data_graphs.csv")
glimpse(df_raw)

# find unique years sorted
df_raw %>% 
  count(year, sort=TRUE) %>% 
  print(n=25)

# visual summary of all cols from summarytools pkg
dfSummary(df_raw)

# duplicate records
nrow(df_raw) - nrow(distinct(df_raw))

# missing data (2018 only)
df_raw %>%
  filter(year == 2018) %>%
  summarise(across(everything(), ~sum(is.na(.))))

df_clean <- df_raw %>%  
  filter(year == 2018) %>% 
  filter(!is.na(gdp)) #remove blank 

glimpse(df_clean)

# plot data and add a geom_smooth lm line 
plot_raw <- df_clean %>% 
  ggplot(aes(x = gdp, 
             y = life_exp)) +
  geom_point(alpha = 0.6, 
             color = "steelblue") +
#  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Scatterplot: Life Expectancy and GDP",
    x = "GDP Per Capita",
    y = "Life Expectancy"
  ) +
  theme_minimal()

plot_raw

#-------------------------------
# Model: Linear Regression
#-------------------------------

lm_gdp <- lm(life_exp ~ gdp, 
             data = df_clean)
summary(lm_gdp)

# pull out coefficients
coef <- summary(lm_gdp)$coefficients

intercept <- round(coef[1, 1], 2)
gdp_coef <- round(coef[2, 1], 6) 

intercept
gdp_coef

# life_exp = 69.48 + 0.000249 x gdp

# Add predicted values to df_clean to re-plot: 
df_clean <- df_clean %>%
  mutate(predicted_life_exp = predict(lm_gdp))

#plot
plot_model <- df_clean %>% 
  ggplot(aes(x = gdp, 
             y = life_exp)) +
  geom_point(alpha = 0.6, 
             color = "steelblue") +
  geom_line(aes(y = predicted_life_exp), 
            color = "darkred")  +
  labs(
    title = "Scatterplot: Life Expectancy and GDP",
    x = "GDP Per Capita",
    y = "Life Expectancy"
  ) +
  theme_minimal()

plot_model

#------------------------------
# Residuals 
#------------------------------

# df of residuals:  

# pull residuals from model output and add to df_clean
resid_df <- df_clean %>%
  mutate(
    .fitted = fitted(lm_gdp),
    .resid = resid(lm_gdp),
    .std_resid = rstandard(lm_gdp)
  )

#plot
plot_residuals <- resid_df %>% 
  ggplot(aes(x = .fitted, 
             y = .resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, 
               color = "red", 
               linetype = "dashed") +
    labs(
      title = "Residuals vs. Fitted Values",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()

plot_residuals

# histogram of residuals: check for normal distribution, no skew
plot_residuals_norm <- resid_df %>%  
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal()

plot_residuals_norm

# QQ plot: check normality of residuals (again) using quantiles, should not fall off the line
plot_qq <- resid_df %>% 
  ggplot(aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()

plot_qq

# Heteroscedasticity: check for equal variance of residuals, should be straight line
resid_df <- resid_df %>%
  mutate(.std_resid = rstandard(lm_gdp))

plot_heteroscedasticity <- resid_df %>% 
  ggplot(aes(x = .fitted, y = sqrt(abs(.std_resid)))) +
    geom_point(alpha = 0.6) +
    geom_smooth(se = FALSE, color = "red") +
    labs(title = "Scale-Location Plot", x = "Fitted Values", y = "√|Standardized Residuals|") +
    theme_minimal()

plot_heteroscedasticity
