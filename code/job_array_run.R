# Author: Megan Ayers
# Date: 10/13/2025
# Function: Intermediate script for job arrays to run specification curve 
#           methods across projects and a pre-saved parameter grid. Shouldn't
#           be run interactively.

rm(list = ls())
source("code/setup.R")  # Loads packages and grabs functions from other scripts.


### Read in command line arguments.
args <- as.list(commandArgs(trailingOnly = TRUE))
names(args) <- c("ate_method", "start", "end", "n_cores")
list2env(args, envir = globalenv())


### Setup.
start <- as.numeric(start); end <- as.numeric(end)
registerDoParallel(cores = n_cores)

info <- readRDS(paste0("data/results/", ate_method, "/param_grid.rds"))
projects <- info$projects
p_grid <- info$p_grid  # Filter to rows for this job.
p_grid <- p_grid[start:end, ]


### Run method for this subset of parameters/projects.
time_start <- proc.time()
res <- run_sc_method(projects, ate_method, p_grid, run_type = "job array")
time_end <- proc.time() - time_start


### Save results from this job & the log with total time for future reference.
saveRDS(res, paste0("data/results/", ate_method, "/job_array_results/",
                    start, "-", end, ".rds"))

this_log <- readLines("data/progress.log")
this_log <- c(this_log, paste("Total job time:", round(time_end[3] / 60, 2),
                              "minutes."))
writeLines(this_log, paste0("data/results/", ate_method,
                            "/job_array_results/time_",
                            start, "-", end, ".txt"))

