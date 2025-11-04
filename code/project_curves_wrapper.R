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


### Function to avoid overwriting grid/job array files accidentally.
safe_write <- function(obj, file, overwrite = FALSE) {
  if (file.exists(file) && !overwrite) {
    ans <- readline(paste0("File '", file,
                           "' already exists. Overwrite? [y/N] "))
    if (!tolower(ans) %in% c("y","yes")) {
      message("Aborted: file not overwritten.")
      return(invisible(FALSE))
    }
  }
  
  if (grepl("\\.txt", file)) {
    write(obj, file)
  } else {
    saveRDS(obj, file)
  }
  return(invisible(TRUE))
}


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
create_grid <- function(ate_method, p_list, time_vars = NULL) {
  
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
    p_grid$pop.size[p_grid$method != "genetic"] <- NA
    p_grid <- p_grid[!duplicated(p_grid), ]
    
  } else {
    p_grid <- expand.grid(grid_helper(p_list, projects),
                          stringsAsFactors = FALSE)
  }
  

  # Shuffle, create groups of combinations based on their expected run time, and 
  # distribute these throughout the grid to help keep run time even across jobs.
  set.seed(1013)
  p_grid <- p_grid[sample(1:nrow(p_grid)), ]
  
  if (!is.null(time_vars)) {
    p_grid <- p_grid %>%
      mutate(group = do.call(paste, lapply(p_grid[time_vars], as.character))) %>%
      group_by(group) %>%
      mutate(idx = row_number() / n()) %>%
      ungroup()
    
    p_grid <- p_grid %>%
      arrange(idx, factor(group, levels = unique(p_grid$group))) %>%
      select(-idx) %>%
      select(-group)
  }

  p_grid <- as.data.frame(p_grid)
  row.names(p_grid) <- NULL
  
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
run_sc_method <- function(projects, ate_method, p_grid, n_cores = 1,
                          run_type = "interactive", save_res = TRUE) {
  
  # Load logic function for the given method.
  # NOTE: This means `ate_method` must match the name of the method directory,
  # and each directory must have a file with this specific names, containing
  # a function named exactly `execute_method()`.
  source(paste0("code/methods/", ate_method, "/logic.R"))
  
  # Iterate through all possible permutations for each project.
  write("", file = "data/progress.log")
  n <- nrow(p_grid)
  curr_results <- data.frame(project_name = character(n), year = integer(n),
                             ATT = numeric(n), lower = numeric(n),
                             upper = numeric(n))
  for (i in 1:nrow(p_grid)) {
    params <- p_grid[i, names(p_grid) != "project"]
    proj_names <- sapply(projects, function(p) p[1])
    project <- projects[proj_names == p_grid[i, "project"]][[1]]
    
    # Call method execution function for the given parameter combination.
    start <- proc.time()
    out <- tryCatch({
      ates_by_year <- execute_method(project_name = project[1],
                                     start_year = as.numeric(project[2]), 
                                     params = params, n_cores = n_cores)
      
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
    
    curr_results[i, ] <- out
  }
  
  # Add parameter information back 
  curr_results <- cbind(curr_results, p_grid[, names(p_grid) != "project"])
  
  if (run_type == "interactive" & save_res) {  # Don't save job array res yet.
    
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
make_sc_curves <- function(projects, ate_method, leftmargin = 5,
                           gc22_comp = FALSE) {
  
  for (project in projects) {
    curr_proj_results <- readRDS(paste0("data/results/", ate_method, "/",
                                        project[1], ".rds"))
    if (gc22_comp & ate_method == "matching") {
      gc22 <- readRDS("data/results/gc22.rds")
      gc22$caliper <- 0.25
      curr_proj_results <- plyr::rbind.fill(
        curr_proj_results,
        gc22 %>% filter(project_name == project[1]))
      highlight <- nrow(curr_proj_results)
    } else {
      highlight <- NULL
    }
    
    curr_proj_results <- curr_proj_results %>%
      filter(!is.na(ATT))
    
    # Plot and save specification curve.
    png(paste0("figs/sc/", ate_method, "/", project[1], ".png"),
        width = 1000, height = 1000)
    create_spec_chart(project_name = project[1], results = curr_proj_results, 
                      spec_order = "increasing", color = "royalblue",
                      leftmargin = leftmargin, highlight = highlight)
    dev.off()
  }

}

