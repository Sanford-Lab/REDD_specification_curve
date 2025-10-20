# Author: Megan Ayers
# Date: 9/1/2025
# Function: Wrapper function that takes a method, list of projects, and method
#           parameter settings to performs all permutations to generate
#           per-project specification curve data for the method.
#           Also includes wrapper for looping over projects to generate plots
#           using method outputs across parameters.

if (interactive()) {
  library(dplyr)
}
source("code/spec_curve/create_spec_chart_function.R")


### Function to add project names into param list for easier looping.
grid_helper <- function(ind_p_list, projects) {
  if (!is.null(ind_p_list)) {
    ind_p_list$project <- sapply(projects, function(p) p[1])
    return(ind_p_list)
  } else {
    return(NULL)
  }
}


### Create grid of parameter permutations x projects, to allow parallelization
### over both parameters and projects, as a data frame.
create_grid <- function(ate_method, p_list) {
  
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
    
    # Remove parameter combinations that are not useful / implemented in
    # underlying packages.
    jk_only <- c("EN", "RF", "seq2seq")
    p_grid$inf_type[p_grid$progfunc %in% jk_only] <- "jackknife"
    p_grid <- p_grid[!duplicated(p_grid), ]
    
  } else if (ate_method == "matching") {
    p_grid <- expand.grid(grid_helper(p_list, projects),
                          stringsAsFactors = FALSE)
    
    # Remove parameter combinations that are not useful (eg where one
    # parameter will be ignored based on the value of another).
    p_grid$distance[p_grid$method == "cem"] <- NA
    p_grid$ratio[p_grid$method == "cem"] <- NA
    p_grid <- p_grid[!duplicated(p_grid), ]
    
  } else {
    p_grid <- expand.grid(grid_helper(p_list, projects),
                          stringsAsFactors = FALSE)
  }
  
  # Shuffle rows of the grid - this helps keep runtime between jobs in the
  # job array roughly similar.
  set.seed(1013)
  p_grid <- p_grid[sample(1:nrow(p_grid)), ]
  
  return(p_grid)
  
}


### Keep a record of current progress (overwrites every time).
update_log <- function(project_name, grid_row, total, time) {
  write(paste0(Sys.time(), ": Worker ", Sys.getpid(), " finished grid row ",
               grid_row, "/", total, " for project ", project_name,
               " after ", round(time / 60, 2),  " minutes."),
        "data/progress.log", append = TRUE)
}


### Main wrapper function for running input method across range of parameters.
run_sc_method <- function(projects, ate_method, p_grid,
                          run_type = "interactive") {
  
  # Load processing and logic functions for the given method.
  # NOTE: This means `ate_method` must match the name of the method directory,
  # and each directory must have files with these specific names, containing
  # functions named exactly `process()` and `execute_method()`.
  source(paste0("code/methods/", ate_method, "/processing.R"))
  source(paste0("code/methods/", ate_method, "/logic.R"))
  
  # Process all project data sets.
  for (project in projects) {
    process(project[1])
    print(paste0("Finished processing ", project[1], " data for ", ate_method,
                 "."))
  }
  
  
  # Iterate through all possible permutations for each project.
  write("", file = "data/progress.log")
  curr_results <- foreach(i = 1:nrow(p_grid), .combine = rbind) %dopar% {
    params <- p_grid[i, names(p_grid) != "project"]
    proj_names <- sapply(projects, function(p) p[1])
    project <- projects[proj_names == p_grid[i, "project"]][[1]]
    
    # Call method execution function for the given parameter combination.
    start <- proc.time()
    out <- tryCatch({
      ates_by_year <- execute_method(project_name = project[1],
                                     start_year = as.numeric(project[2]), 
                                     params = params)
      
      # Retrieve the ATT, lower and upper CI bounds for the year 2022.
      # NOTE: May want to make year an input variable, or a project-specific
      # feature saved in the `projects` list with name and start year.
      result_2022 <- ates_by_year %>% filter(year == 22)
      data.frame(project_name = project[1], year = project[2],
                 ATT = result_2022$coef, lower = result_2022$lower,
                 upper = result_2022$upper)
      
    }, error = function(e) {
      write(paste0(Sys.time(), ": Error with grid row ", i, "/", nrow(p_grid),
                   " for project ", project[1], ": ",
                   conditionMessage(e)),
            "data/progress.log", append = TRUE)
      data.frame(project_name = project[1], year = project[2],
                 ATT = NA, lower = NA, upper = NA)
    })
    
    end <- proc.time() - start
    update_log(project[1], i, nrow(p_grid),  # Record progress
               as.numeric(round(end[3], 2)))  
    
    return(out)
  }
  
  # Add parameter information back 
  curr_results <- cbind(curr_results, p_grid[, names(p_grid) != "project"])
  
  if (run_type == "interactive") {  # Do not save individual job array results.
    
    # Record all ATT estimates separately for each project.
    for (project in projects) {
      these <- curr_results[curr_results$project_name == project[1], ]
      saveRDS(these, paste0("data/results/", ate_method, "/", project[1],
                            ".rds"))
    }
  }
  
  invisible(curr_results)
}


### Make specification curves for each project.
make_sc_curves <- function(projects, ate_method, leftmargin = 5) {
  
  for (project in projects) {
    curr_proj_results <- readRDS(paste0("data/results/", ate_method, "/",
                                        project[1], ".rds"))
    curr_proj_results <- curr_proj_results %>%
      filter(!is.na(ATT))
    
    # Plot and save specification curve.
    png(paste0("figs/sc/", ate_method, "/", project[1], ".png"),
        width = 800, height = 800)
    create_spec_chart(project_name = project[1], results = curr_proj_results, 
                      spec_order = "increasing", color = "royalblue",
                      leftmargin = leftmargin)
    dev.off()
  }

}

