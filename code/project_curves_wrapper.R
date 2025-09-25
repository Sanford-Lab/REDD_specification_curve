# Author: Megan Ayers
# Date: 9/1/2025
# Function: Takes a method, list of projects, and method parameter settings.
#           Performs all permutations to generate per-project specification
#           curves for the method, and saves the associated data frames.

library(dplyr)
source(here("code", "spec_curve", "create_spec_chart_function.R"))


# Function to add project names into param list for easier looping.
grid_helper <- function(ind_p_list, projects) {
  ind_p_list$project <- sapply(projects, function(p) p[1])
  return(ind_p_list)
}


# Keep a record of current progress (overwrites every time).
update_log <- function(project_name, grid_row, total) {
  write(paste0(Sys.time(), ": Worker ", Sys.getpid(), " finished grid row ",
               grid_row, "/", total, " for project ", project_name, "."),
        "data/progress.log", append = TRUE)
}


### Main wrapper function.
make_sc_curves <- function(projects, ate_method, p_list, plot_only = FALSE,
                           leftmargin = 5) {
  
  ### This block will perform all preprocessing and method execution. 
  if (!plot_only) { 
    
    # Load processing and logic functions for the given method.
    # NOTE: This means `ate_method` must match the name of the method directory,
    # and each directory must have files with these specific names, containing
    # functions named exactly `process()` and `execute_method()`.
    source(here("code", "methods", ate_method, "processing.R"))
    source(here("code", "methods", ate_method, "logic.R"))
    
    # Process all project data sets.
    for (project in projects) {
      process(project[1])
      print(paste0("Finished processing ", project[1], " data for ", ate_method,
                   "."))
    }
    
    # Create grid of parameter permutations x projects, to allow parallelization
    # over both parameters and projects.
    if (ate_method == "synthetic_controls") {
      # Different flavors of synthetic controls need different parameters, but
      # want to see them in the same plot.
      p_grid_g <- expand.grid(grid_helper(p_list$gysnth, projects),
                              stringsAsFactors = FALSE)
      p_grid_m <- expand.grid(grid_helper(p_list$microsynth, projects),
                              stringsAsFactors = FALSE)
      p_grid_a <- expand.grid(grid_helper(p_list$augsynth, projects),
                              stringsAsFactors = FALSE)
      p_grid <- plyr::rbind.fill(list(p_grid_g,  # Pads non-shared columns
                                      p_grid_m, p_grid_a))
    } else {
      p_grid <- expand.grid(grid_helper(p_list, projects),
                            stringsAsFactors = FALSE)
    }
    
    
    # Iterate through all possible permutations for each project.
    write("", file = "data/progress.log")
    curr_results <- foreach(i = 1:nrow(p_grid), .combine = rbind) %dopar% {
      params <- p_grid[i, names(p_grid) != "project"]
      proj_names <- sapply(projects, function(p) p[1])
      project <- projects[proj_names == p_grid[i, "project"]][[1]]
      
      # Call method execution function for the given parameter combination.
      # print(paste0("Starting ", ate_method, " for ", project[1],
      #              " with parameter setting ", i, "/", nrow(p_grid), "."))
      ates_by_year <- execute_method(project_name = project[1],
                                     start_year = as.numeric(project[2]), 
                                     params = params)
      
      update_log(project[1], i, nrow(p_grid))  # Record progress
      
      # Retrieve the ATT, lower and upper CI bounds for the year 2022.
      # NOTE: May want to make year an input variable, or a project-specific
      # feature saved in the `projects` list with name and start year.
      result_2022 <- ates_by_year %>% filter(year == 22)
      
      return(data.frame(project_name = project[1], year = project[2],
                        ATT = result_2022$coef, lower = result_2022$lower,
                        upper = result_2022$upper))
    }
    
    # Add parameter information back 
    curr_results <- cbind(curr_results, p_grid[, names(p_grid) != "project"])
  
    # Record all ATT estimates separately for each project.
    for (project in projects) {
      these <- curr_results[curr_results$project_name == project[1], ]
      saveRDS(these, paste0("data/results/", ate_method, "/", project[1],
                            ".rds"))
    }
  }
  

  ### Make specification curves for each project.
  for (project in projects) {
    curr_proj_results <- readRDS(paste0("data/results/", ate_method, "/",
                                        project[1], ".rds"))
    
    # Plot and save specification curve.
    png(paste0("figs/sc/", ate_method, "/", project[1], ".png"),
        width = 800, height = 800)
    create_spec_chart(project_name = project[1], results = curr_proj_results, 
                      spec_order = "increasing", color = "royalblue",
                      leftmargin = leftmargin)
    dev.off()
  }
    
}

