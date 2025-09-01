# Author: Nick Wu and Henry Chen
# Date: 4/10/2024
# Function:

library(MatchIt)
library(grf)
library(estimatr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(here)

#-------------------------------------------------------------------------------
# processing the data on all projects

# get a list of all (project_name, start_year)
source(here('code', 'Projects', 'universal_list_of_projects.R'))
projects <- get_projects()

# process the datasets from all projects:
source(here("code", "methods", "causal_forests", "forest_processing.R"))
for (project in projects) {process_forest_data(project[1])}

#-------------------------------------------------------------------------------
# creating the causal_forest_model function to apply to projects

causal_forest_model <- function(project_name, start_year, outcome_var = "treecover_remaining", 
                                covariates = c("aspect", "elevation", "hillshade", "slope", "defo_distance", "accessibility"), 
                                num_trees = 1000, min.node.size = 10) {
  
# Load the data specific to the project
load(paste0("data/processed/", project_name, "/dat_forest.Rdata"))
  
covariates_columns <- c("aspect", "elevation", "hillshade", "slope", "defo_distance", "accessibility")

# Train causal forest model 
model <- causal_forest(X = dat_long[, covariates_columns], 
                         Y = dat_long$treecover_remaining,
                         W = dat_long$treated,
                         num.trees = num_trees,
                         min.node.size = min.node.size)  
  
  # calculate estimated treatment effect
  ate <- average_treatment_effect(model, target.sample = "overlap")
  std_err <- sqrt(var(ate))
  
  # Calculate 95% confidence interval
  lower <- ate - 1.96 * std_err
  upper <- ate + 1.96 * std_err
  
  # Return the estimate and the confidence interval
  return(c(ate = ate, lower = lower, upper = upper))
}

#-------------------------------------------------------------------------------
# creating and applying combinations

# Define lists of parameters to try
covariate_sets <- list(
  c("aspect", "defo_distance", "accessibility")
)
num_trees_values <- c(500,1000)
min_node_size_values <- c(10)

# Initialize an empty list to store results
plotting_results <- list()

# Iterate through projects
for (project in projects) {
  # Initialize a dataframe to store results for the current project
  curr_proj_results <- data.frame(
    project_name = character(),
    year = numeric(),
    covariates = character(),
    num_trees = numeric(),
    min.node.size = numeric(),
    ATT = numeric(),
    lower = numeric(),
    upper = numeric()
  )
  
  # Iterate through parameter combinations
  for (covariates in covariate_sets) {
    for (num_trees in num_trees_values) {
      for (min.node.size in min_node_size_values) {
        # Call causal_forest_model function with the specified combination of parameters
        result <- causal_forest_model(project_name = project[1], 
                                      start_year = as.numeric(project[2]),  # Ensure year is numeric
                                      covariates = covariates,
                                      num_trees = num_trees,
                                      min.node.size = min.node.size)
        
        # Extract estimate, lower bound, and upper bound from the result
        ate <- result[1]
        lower <- result[2] 
        upper <- result[3]  
        
        # Store results in the dataframe
        curr_proj_results <- rbind(curr_proj_results, data.frame(
          project_name = project[1],
          year = as.numeric(project[2]),  # Ensure year is numeric
          covariates = toString(covariates),  
          num_trees = num_trees,
          min.node.size = min.node.size,
          ATT = ate,
          lower = lower,
          upper = upper
        ))
      }
    }
  }
  
  # Append results for the current project to the list
  plotting_results[[length(plotting_results) + 1]] <- curr_proj_results
}

#-------------------------------------------------------------------------------
# Plotting the results:

# load matching spec curve function
source(here("code", "spec_curve", "create_spec_chart_function.R"))

# return matching specification curve but looped.
for (i in 1:length(projects)) {
  create_spec_chart(project_name = projects[[i]], 
                     plotting_results[[i]], 
                     spec_order = "increasing", 
                     color = "darkorange")
}


  
  
  
