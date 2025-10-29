# Author: Megan Ayers
# Date: 9/1/2025
# Function: Interactive script for setting projects, methods, and parameters to
#           run for specification curves.

rm(list = ls())
source("code/setup.R")  # Loads packages and grabs functions from other scripts.


# ----- STEP 1: Set computational parameters ----------------------------------
run_type <- "job array"   # Set to either "interactive" or "job array"
n_cores <- 1                # Number of cores per job / on the local machine


# Only worry about these if run_type == "job array".
gb_per_core <- 4
time <- "08:30:00"
rows_per_job <- 15


# ----- STEP 2: Set the method and list of parameter settings -----------------

### Uncomment / adjust p_list to run for MATCHING.
# ate_method <- "matching"
# 
# all_covars <- c("treecover_past", "accessibility", "accessibility_walking_only",
#                 "aspect", "elevation", "slope")
# loo_covars <- lapply(1:length(all_covars), function(i) all_covars[-i])
# p_list <- list(method = c("nearest", "cem", "genetic"),
#                distance = c("logit", "mahalanobis", "euclidean"),
#                pop.size = c(50, 100, 500),
#                ratio = c(1, 3, 5),
#                covariates = loo_covars)
# time_vars = c("method", "pop.size")  # These variables greatly affect runtime


### Uncomment / adjust p_list to run for SYNTHETIC CONTROLS.
ate_method <- "synthetic_controls"

all_covars <- c("accessibility", "accessibility_walking_only", "aspect",
                "elevation", "slope")
loo_covars <- lapply(1:length(all_covars), function(i) all_covars[-i])
p_list_g <- list(sc_method = "gsynth",
                 force = c("none", "unit", "time", "two-way"),
                 estimator = c("ife"),
                 r = c(1, 3, 5, "cv"))
p_list_m <- list(sc_method = "microsynth",
                 covariates = loo_covars)
p_list_a <- list(sc_method = "augsynth",
                 inf_type = c("conformal", "jackknife"),
                 covariates = loo_covars,
                 progfunc = c("None", "EN", "Ridge", "RF", "MCP", "CITS",
                              "seq2seq"))
p_list <- list("gysnth" = p_list_g, "microsynth" = p_list_m,
               "augsynth" = p_list_a)

time_vars = c("sc_method", "progfunc")  # These variables greatly affect runtime



# ----- STEP 3: Set list of projects to consider and process ------------------
projects <- get_projects()


# Process all project data sets.
source(paste0("code/methods/", ate_method, "/processing.R"))
for (project in projects) {
  process(project[1], overwrite = FALSE)
  print(paste0("Finished processing ", project[1], " data for ", ate_method,
               "."))
}


# ----- STEP 4: Run this code to run the method or create a job list ----------

p_grid <- create_grid(ate_method, p_list,  # Creates parameter grid
                      time_vars = time_vars)


if (run_type == "job array") {
  job_list_file <- paste0("data/results/", ate_method, "/job_list.txt")
  n_jobs <- ceiling(nrow(p_grid) / rows_per_job)
}


### Run the method across the entire grid if running interactively.
if (run_type == "interactive") {
  registerDoParallel(cores = n_cores)
  run_sc_method(projects, ate_method, p_grid)   # Runs methods


### Or, initialize a job array run.
### *** Job array must be run via terminal, follow the printed instructions.***
} else {
  safe_write(list(p_grid = p_grid, projects = projects),  # Save grid for jobs.
             paste0("data/results/", ate_method, "/param_grid.rds"))
  
  # Create job list. 
  base_str <- "ml miniconda; conda activate r-geo; Rscript --vanilla code/job_array_run.R"
  job_list <- ""
  for (i in 1:n_jobs) {
    start <- (i - 1) * rows_per_job + 1
    end <- min(rows_per_job * i, nrow(p_grid))
    this <- paste0(base_str, " \"", ate_method, "\" ", start, " ", end,
                   " ", n_cores, "\n")
    job_list <- paste0(job_list, this)
  }
  safe_write(job_list, job_list_file)
  dir.create(paste0("data/results/", ate_method, "/job_array_results"))
}


# ----- STEP 5: (JOB ARRAY ONLY) Run job array from terminal ------------------

# (a) Open shell terminal on Bouchet. 

# (b) Navigate to this project directory.

# (c) Load the dSQ module by running: `ml dSQ`

# (d) Create the dSQ shell file using the job list created above by running the
#     result printed by this line:
cat(paste0("\ndsq --job-file ", job_list_file, " --mem-per-cpu ",
           gb_per_core, "g -t ", time, " --cpus-per-task ", n_cores,
           " --partition day ",
           "--batch-file data/results/", ate_method, "/dsq-jobfile-",
           substr(Sys.time(), 1, 10),
           ".sh ", "--output cluster_logs/dsq-jobfile-%A_%a-%N.out ",
           "--status-dir cluster_logs/ ", "\n"))
dir.create("cluster_logs")

# (e) Then sbatch the sh script created by that command (follow the instructions
#     that the last command gave you in the terminal.) You can check on the
#     job array status by running `squeue --me`.



# ----- STEP 6: Run this code to make project specification curves ------------

if (run_type == "job array") {
  
  job_list <- readLines(paste0("data/results/", ate_method, "/job_list.txt"))
  
  # Save a copy of the full job list - will update job_list.txt to rerun any
  # failed jobs.
  if (!file.exists(paste0("data/results/", ate_method, "/job_list_all.txt"))) {
    write(job_list, paste0("data/results/", ate_method, "/job_list_all.txt"))
  }
  
  job_res_files <- list.files(paste0("data/results/", ate_method,
                                     "/job_array_results/"), full.names = TRUE)
  job_res_files <- grep("\\.rds", job_res_files, value = TRUE)
  
  # Stitch job array results together.
  if (length(job_res_files) == n_jobs) {
    all_results <- readRDS(job_res_files[1])
    for (f in job_res_files[2:length(job_res_files)]) {
      all_results <- rbind(all_results,
                           readRDS(f))
    }
  } else {
    job_list <- job_list[job_list != ""]
    jobs <- sapply(strsplit(job_list, " "), function(s) s[10])
    completes <- gsub(".*\\/([0-9]+)-[0-9]+\\.rds", "\\1", job_res_files)
    job_list <- job_list[which(!jobs %in% completes)]
    write(job_list, paste0("data/results/", ate_method, "/job_list.txt"))
    stop("Some jobs did not complete. The job list has been updated - repeat Step 5 after adjusting compute parameters in Step 1 if necessary.")
  }
  
  # Save results for each project - across all parameters - separately.
  for (project in projects) {
    these <- all_results[all_results$project_name == project[1], ]
    saveRDS(these, paste0("data/results/", ate_method, "/", project[1],
                          ".rds"))
  }
  
}

# Generate specification curves across projects.
make_sc_curves(projects, ate_method, leftmargin = 5)

