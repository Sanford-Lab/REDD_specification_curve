#### Practice for random/causal forests on adpml project

rm(list = ls())

# load the dat_long table
project_name = "adpml"
load(paste0("data/processed/", project_name, "/dat_matching.Rdata"))

# grf package
library(grf)

# train forest
set.seed(123) 
train_indices <- sample(1:nrow(dat_long), 0.8 * nrow(dat_long))
train_data <- dat_long[train_indices, ]
test_data <- dat_long[-train_indices, ]

covariates_columns <- c("aspect", "elevation", "hillshade", "slope", "defo_distance", "accessibility")
model <- causal_forest(X = train_data[, covariates_columns], 
                       Y = train_data$treecover_remaining,
                       W = train_data$treated,
                       num.trees = 1000,
                       )

# calculate estimated treatment propensities
propensities <- predict(model, type = "propensity")

# dealing with extreme propensities
extreme_indices <- which(propensities <= 0.05 | propensities >= 0.95)

if (length(extreme_indices) > 0) {
# show warning message
warning("Estimated treatment propensities take values between 0 and 1 and in particular get very close to 0 and 1.")
  
# filter data to exclude extreme propensities
filtered_data <- dat_long[-extreme_indices, ]
  
# retrain causal forest with filtered_data
filtered_model <- causal_forest(X = filtered_data[, covariates_columns], 
                                Y = filtered_data$treecover_remaining,
                                W = filtered_data$treated,
                                num.trees = 1000)
} else {
# continue with original model if no extreme propensities
filtered_model <- model
}

# Average Treatment Effect
ate <- average_treatment_effect(filtered_model, target.sample = "overlap")
print(ate)

# Variable Importance
var_importance <- variable_importance(filtered_model)
print(var_importance)


