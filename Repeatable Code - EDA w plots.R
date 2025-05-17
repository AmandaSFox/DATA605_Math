# REPEATABLE, TWEAKABLE EDA 

# Load libraries
library(tidyverse)
library(summarytools)

# Load your dataset
df_raw <- read_csv("gapminder_data_graphs.csv") # CHANGE FILENAME

# ----- BASIC OVERVIEW -----

# Glimpse structure
glimpse(df_raw)

# Summary of all columns
dfSummary(df_raw)

# Duplicate rows
n_duplicates <- nrow(df_raw) - nrow(distinct(df_raw))
cat("Duplicate rows:", n_duplicates, "\n")

# Unique years (or other key categorical var)
df_raw %>%
  count(year, sort = TRUE)

# ----- MISSING DATA -----

# Missing value count per column (filtered to 2018 for example)
df_raw %>%
  filter(year == 2018) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  print()

# Missing value % per column
df_raw %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  round(1) %>%
  print()

# ----- DATA CLEANING / FILTERING -----

# Example clean version: just 2018, remove missing GDP
df_clean <- df_raw %>%
  filter(year == 2018, !is.na(gdp))

# Quick check of cleaned data
glimpse(df_clean)

# ----- NUMERIC SUMMARY -----

df_clean %>%
  summarise(across(where(is.numeric), list(
    min = ~min(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ))) %>%
  print()

# ----- CATEGORICAL SUMMARY -----

df_clean %>%
  select(where(~is.character(.) || is.factor(.))) %>%
  map(~table(.) %>% sort(decreasing = TRUE) %>% head(5)) %>%
  print()

# ----- OPTIONAL: Save cleaned version -----

saveRDS(df_clean, "df_clean_2018.rds")
