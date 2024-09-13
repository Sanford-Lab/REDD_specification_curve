# Author: Henry Chen
# Date: 10/24/2023
# Function: Takes list of projects and matching permutations. Performs all permutations. 
#           then stores it in a list of dataframes. Finally creates specification plot
#           of all these permutations.

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

#-------------------------------------------------------------------------------
# Applying all combinations of matching logic on projects:

# List of combinations to try
methods <- c("nearest", 
             # "genetic",    # commented this out bc it took too long to test.
             "cem"
)
distances <- c("logit", "mahalanobis", "euclidean")
ratios <- c(1,3,5)

# Dataframe to store results 
plotting_results <- list()

source(here('code', 'methods', 'matching', 'matching_logic.R'))

# Iterate through all possible permutations.
for (project in projects) {
  curr_proj_results <- data.frame(
    project_name = character(),
    year = numeric(),
    method = character(),
    distance = character(),
    ratio = numeric(),
    ATT = numeric(),
    lower = numeric(),
    upper = numeric()
  )
  for (method in methods) {
    for (distance in distances) {
      for (ratio in ratios) {
        # Call match_data function with a given combination
        print(c(method, distance, ratio, project[1], project[2]))
        ates_by_year <- match_data(project_name = project[1], #first element of the vector
                                   start_year = as.numeric(project[2]), #second element of the vector
                                   method = method, 
                                   distance = distance, 
                                   ratio = ratio)
        
        # Retrieve the ATT, lower and upper CI bounds for the year 2022
        result_2022 <- ates_by_year %>% filter(year == 22)
        
        # Add the results to the dataframe
        curr_proj_results <- rbind(curr_proj_results, data.frame(
          project_name = project[1],
          year = project[2],
          method = method,
          distance = distance,
          ratio = ratio,
          ATT = result_2022$coef,
          lower = result_2022$lower,
          upper = result_2022$upper
        ))
      }
    }
  }
  # print(curr_proj_results)
  plotting_results[[length(plotting_results) + 1]] <- curr_proj_results
}



#-------------------------------------------------------------------------------
# Plotting the results:

# load matching spec curve function
source(here("code", "spec_curve", "create_spec_chart_function.R"))

# return matching specification curve but looped.
for (i in 1:length(projects)) {
  creates_spec_chart(project_name = projects[[i]], 
                      plotting_results[[i]], 
                      spec_order = "increasing", 
                      color = "royalblue")
}

