# Author: Megan Ayers
# Date: 9/1/2025
# Function: Takes a method, list of projects, and method parameter settings.
#           Performs all permutations to generate per-project specification
#           curves for the method, and saves the associated data frames.

library(here)
library(dplyr)
source(here("code", "spec_curve", "create_spec_chart_function.R"))


make_sc_curves <- function(projects, ate_method, p_list) {
  
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
    saveRDS(curr_proj_results, paste0("data/results/", ate_method, "/",
                                      project[1], ".rds"))
    
    
    # Plot and save specification curve.
    png(paste0("figs/sc/", ate_method, "/", project[1], ".png"),
        width = 800, height = 800)
    create_spec_chart(project_name = project[1], curr_proj_results, 
                      spec_order = "increasing", color = "royalblue")
    dev.off()
    
  }
}

