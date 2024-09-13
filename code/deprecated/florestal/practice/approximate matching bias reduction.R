# Purpose: Approximate matching bias reduction
# Author: Henry Chen
# Date: 6/29/2023

# when exact matching can't occur...we necessarily create matching discrepancies, 
# which is simply another way of saying that the covariates are not perfectly matched 
# for every unit.

library(tidyverse)
library(haven)
library(stargazer)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/",
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

training_bias_reduction <- read_data("training_bias_reduction.dta") %>%
  mutate(
    Y1 = case_when(Unit %in% c(1,2,3,4) ~ Y),
    Y0 = c(4,0,5,1,4,0,5,1))

train_reg <- lm(Y ~ X, training_bias_reduction)

training_bias_reduction <- training_bias_reduction %>%
  mutate(u_hat0 = predict(train_reg))

stargazer(train_reg, type = "text")

# we can now use this prediction for u_hat0 in calculating the ATT to account
# for bias. 
