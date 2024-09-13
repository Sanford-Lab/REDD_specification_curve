# Author: Henry Chen
# Date: 2/6/2024
# Function: applies matching but leaves out one covariate at a time

library(here)
library(dplyr)

# Get list of all (project name, start year)
source(here('code', 'Projects', 'universal_list_of_projects.R'))
projects <- get_projects()


# Processing all the datasets:
source(here("code", "methods", "matching", "matching_processing.R"))

for (project in projects) {
  process_matching_data(project[1])
}

# Dataframe to store results 
plotting_results <- list()


# list of covariates
covariates <- c("treecover_past", "accessibility", "accessibility_walking_only", "aspect", "elevation", "slope")

source(here('code', 'methods', 'matching', 'matching_logic.R'))

# 
# ates_by_year <- match_data(project_name = "adpml", #first element of the vector
#                            start_year = 9)


# Iterate through all possible permutations.
for (project in projects) {
  curr_proj_results <- data.frame(
    project_name = character(),
    year = numeric(),
    covariate = character(),
    ATT = numeric(),
    lower = numeric(),
    upper = numeric()
  )
  for (covariate in covariates) {
    # generate list minus "covariate"
    curr_list <- covariates[covariates != covariate]

    ates_by_year <- match_data(project_name = project[1], #first element of the vector
                               start_year = as.numeric(project[2]),
                               covariates = curr_list) #second element of the vector
                               
    
    # Retrieve the ATT, lower and upper CI bounds for the year 2022
    result_2022 <- ates_by_year %>% filter(year == 22)
    
    # Add the results to the dataframe
    curr_proj_results <- rbind(curr_proj_results, data.frame(
      project_name = project[1],
      year = project[2],
      covariate = covariate,
      ATT = result_2022$coef,
      lower = result_2022$lower,
      upper = result_2022$upper
    ))
  }
  # print(curr_proj_results)
  plotting_results[[length(plotting_results) + 1]] <- curr_proj_results
}

source(here("code", "spec_curve", "create_spec_chart_function.R"))

# return matching specification curve but looped.
for (i in 1:length(projects)) {
  print(plotting_results[[i]])
  create_spec_chart(project_name = projects[[i]], 
                     plotting_results[[i]], 
                     spec_order = "increasing", 
                     color = "royalblue")
}
