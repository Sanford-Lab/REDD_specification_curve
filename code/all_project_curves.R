# Author: Megan Ayers
# Date: 9/1/2025
# Function: Takes a method, list of projects, and method parameter settings.
#           Performs all permutations to generate per-project specification
#           curves for the method, and saves the associated data frames.

rm(list = ls())
library(here)
library(dplyr)
source(here('code', 'Projects', 'universal_list_of_projects.R'))


# **** Eventually this part should exist in a separate interactive run script *** 
# Get list of all projects (project name, start year).
projects <- get_projects()[1:3]
ate_method <- "matching"
p_list <- list(method = c("nearest", "cem"),
               distance = c("logit", "mahalanobis", "euclidean"),
               ratio = c(1, 3, 5))
# *********



# Load processing and logic functions for the given method.
source(here("code", "methods", ate_method, "processing.R"))
source(here("code", "methods", ate_method, "logic.R"))

# Process all project data sets.
for (project in projects) {
  process(project[1])
  print(paste0("Finished processing ", project[1], " data for matching."))
}

# Create grid of parameter permutations.
p_grid <- expand.grid(p_list, stringsAsFactors = FALSE)


# Iterate through all possible permutations for each project.
results <- vector("list", length(projects))
names(results) <- sapply(1:length(projects), function(i) projects[[i]][1])
for (project in projects) {
  curr_proj_results <- data.frame(project_name = character(), year = numeric(),
                                  ATT = numeric(), lower = numeric(),
                                  upper = numeric())
  for (i in 1:nrow(p_grid)) {
    params <- p_grid[i, ]
    
    # Call method execution function for the given parameter combination.
    print(paste0("Starting ", ate_method, " for ", project[1],
                 " with parameter setting ", i, "/", nrow(p_grid), "."))
    ates_by_year <- execute_method(project_name = project[1],
                                   start_year = as.numeric(project[2]), 
                                   params = params)
    
    # Retrieve the ATT, lower and upper CI bounds for the year 2022.
    result_2022 <- ates_by_year %>% filter(year == 22)
    curr_proj_results <- rbind(curr_proj_results, data.frame(
      project_name = project[1],
      year = project[2],
      ATT = result_2022$coef,
      lower = result_2022$lower,
      upper = result_2022$upper
    ))
  }

  # Record all ATT estimates for this project + parameter settings used.
  curr_proj_results <- cbind(curr_proj_results, p_grid)
  results[[project[1]]] <- curr_proj_results
}



#-------------------------------------------------------------------------------
# Plotting the results:

# load matching spec curve function
source(here("code", "spec_curve", "create_spec_chart_function.R"))

# return matching specification curve but looped.
for (i in 1:length(projects)) {
  create_spec_chart(project_name = projects[[i]],  results[[i]], 
                    spec_order = "increasing", color = "royalblue")
}

