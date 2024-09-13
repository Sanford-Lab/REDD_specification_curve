# Purpose: Practicing exact matching procedure based on Cunningham book
# Author: Henry Chen
# Date: 6/28/2023

# What is matching? ------------------------------------------------------------
# what if we filled in the missing potential outcome for each treatment unit using 
# a control group unit that was “closest” to the treatment group unit for some X 
# confounder? This would give us estimates of all the counterfactuals from which 
# we could simply take the average over the differences. As we will show, this 
# will also achieve covariate balance. This method is called matching.
# usually for ATT but can also be extended for ATE


# Exact matching -- code to show how treat and control pops are inherently different
library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/",
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

training_example <- read_data("training_example.dta") %>%
  slice(1:20)

ggplot(training_example, aes(x = age_treat)) +
  stat_bin(bins = 10, na.rm = TRUE)

ggplot(training_example, aes(x = age_control)) + 
  geom_histogram(bins = 10, na.rm = TRUE)
