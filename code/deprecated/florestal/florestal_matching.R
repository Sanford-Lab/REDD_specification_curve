# Purpose: implementing different matching for FSM
# Author: Henry Chen
# Date: 7/5/2023

library(MatchIt)
library(estimatr)
library(modelsummary)

load("data/processed/florestal/dat_matching.Rdata")

datasummary_skim(data = dat_long %>% ungroup() %>% 
                   select(treecover_2000, treecover_remaining, accessibility, accessibility_walking_only, aspect, elevation, slope, cum_loss, loss, defo_distance))

dat_long_fsm <- dat_long_fsm %>% 
  filter(year == 8) %>% 
  select(ID, treecover_remaining) %>%
  rename(treecover_2008 = treecover_remaining) %>% 
  right_join(dat_long_fsm)

dat_long_09 <- dat_long_fsm %>% filter(year == 9)

# nearest-neighbor matching
# ------------------------------------------------------------------------------
m_out <- matchit(treated ~ treecover_2008 + accessibility + accessibility_walking_only
                 + aspect + elevation + slope,
                 data = dat_long_09, method = "nearest", distance = "logit", ratio = 5)

m_data <- match.data(m_out) # table summary of results

summary(m_out) # numerical summary of the matches
plot(m_out, type = "qq") # visualize the matches

# determine the ATE, regressing either the tree_cover or loss on whether treated or not
m_ate_nn <- lm_robust(treecover_remaining ~ treated, 
                   data = m_data,
                   weights = m_data$weights)

summary(m_ate_nn)

m_data <- m_data %>% select(ID, weights)

dat_long_fsm <- dat_long_fsm %>%
  left_join(m_data) %>% drop_na(weights)


# Coarsened exact matching (CEM) -- idea: sometimes its possible to do exact matching
# once you coarsen the data enough. If we coarsen the data, meaning we create categorical 
# variables (e.g., 0- to 10-year-olds, 11- to 20-year olds), then oftentimes we can 
# find exact matches.
# ------------------------------------------------------------------------------

library(cem)
library(tidyverse)
library(estimatr)

m_cem <- matchit(treated ~ treecover_2000 + accessibility + accessibility_walking_only
                 + aspect + elevation + slope,
                 data = dat_long_fsm, method = "cem", distance = "logit", ratio = 5)

m_cem.data <- match.data(m_cem)

summary(m_cem)
plot(m_cem, type = "qq")

# determine the ATE, regressing either the tree_cover or loss on whether treated or not
m_ate <- lm_robust(treecover_remaining ~ treated, 
                   data = m_cem.data,
                   weights = m_cem.data$weights)

summary(m_ate)



## could also create graph of all the ATEs for each year one by one and see the trends
## which should resemble synthetic controls (?)
# ------------------------------------------------------------------------------

ates_by_year <- list()
years <- list(9:22)

# loop through all 22 years and run the regression filtering for that specific year only
for (i in years[[1]]) {
  dat_i <- dat_long_fsm %>% filter(year == i)
  
  m_nn <- matchit(treated ~ treecover_2008 + accessibility + accessibility_walking_only
                   + aspect + elevation + slope,
                   data = dat_i, method = "nearest", distance = "logit", ratio = 10)
  
  m_nn.data <- match.data(m_nn)
  
  ate_i <- lm_robust(loss ~ treated, 
                     data = m_nn.data,
                     weights = m_nn.data$weights)
  
  # get the coefficient for the treated variable in the regression
  coef <- coef(ate_i)[2]
  print(coef)
  
  # save the coef to the ates_by_year list
  ates_by_year <- append(ates_by_year, coef)
}
  
# plot the graph
plot(years[[1]], ates_by_year, type = "b", pch = 16,
     xlab = "Years since 2000", ylab = "ATE (treecover remaining)", main = "ATEs by Year for FSM")
abline(v = 9, col = "red", lwd = 2)
