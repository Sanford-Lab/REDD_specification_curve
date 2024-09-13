# Purpose: implementing matching for agrocortex
# Author: Henry Chen
# Date: 7/13/2023

# note* be75 was changed to elevation from fsm -> agrocortex

library(MatchIt)

load("data/processed/agrocortex/dat_matching_500.Rdata")

# nearest-neighbor matching
# ------------------------------------------------------------------------------
m_out <- matchit(treated ~ treecover_2000 + accessibility + accessibility_walking_only
                 + aspect + elevation + slope,
                 data = dat_long_agrocortex, method = "nearest", distance = "logit", ratio = 5)

m_data <- match.data(m_out) # table summary of results

summary(m_out) # numerical summary of the matches
plot(m_out, type = "qq") # visualize the matches

# determine the ATE, regressing either the tree_cover or loss on whether treated or not
m_ate_nn <- lm_robust(treecover_remaining ~ treated, 
                      data = m_data,
                      weights = m_data$weights)

summary(m_ate_nn)


## could also create graph of all the ATEs for each year one by one and see the trends
## which should resemble synthetic controls (?)
# ------------------------------------------------------------------------------

ates_by_year <- list()
years <- list(1:22)

# loop through all 22 years and run the regression filtering for that specific year only
for (i in years[[1]]) {
  dat_i <- dat_long_agrocortex %>% filter(year == i)
  
  m_nn <- matchit(treated ~ treecover_2000 + accessibility + accessibility_walking_only
                  + aspect + elevation + slope,
                  data = dat_i, method = "nearest", distance = "logit", ratio = 10)
  
  m_nn.data <- match.data(m_nn)
  
  ate_i <- lm_robust(treecover_remaining ~ treated, 
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
     xlab = "Years since 2000", ylab = "ATE (treecover remaining)", main = "ATEs by Year for Agrocortex")
abline(v = 9, col = "red", lwd = 2)
