# Author: Megan Ayers
# Date: 11/3/2025
# Function: Run method replication for Guizar-Coutino et al. (2022),
#           West et al. (2023), and Tang et al. (2025)

rm(list = ls())
source("code/setup.R")
projects <- get_projects()[1]
library(pensynth)
library(doParallel)
registerDoParallel(cores = 5)


### ------------------- Guizar-Coutino et al. (2022) --------------------------
# source("code/methods/matching/logic.R") 
# params <- list(method = "nearest", distance = "mahalanobis", pop.size = NA,
#                ratio = 1, covariates = list(c("elevation", "slope",
#                                               "accessibility", "defo_distance")),
#                std.caliper = TRUE)
# params$caliper <- list(rep(0.25, length(params$covariates[[1]])))
# names(params$caliper[[1]]) <- params$covariates[[1]]
# 
# p_grid <- create_grid("matching", params, projects)
# 
# gc22 <- run_sc_method(projects, "matching", p_grid, save_res = FALSE)
# # saveRDS(gc22, "data/results/gc22.rds")


### ------------------- West et al. (2023) ------------------------------------
### TODO: **** Update covariates once more are pulled from GEE. Refer to 
###            West et al. (2023) supplementary materials, Table S2. ***
# source("code/methods/synthetic_controls/logic.R")
# params <- list(microsynth = list(sc_method = "microsynth", force = NA,
#                                  estimator = NA, r = NA, inf_type = NA, progfunc = NA,
#                                  covariates = list(c("accessibility", "elevation", "slope"))))
# p_grid <- create_grid("synthetic_controls", params, projects)
# 
# west23 <- run_sc_method(projects, "synthetic_controls", p_grid, save_res = FALSE)
# saveRDS(west23, "data/results/west23.rds")


### ------------------- Tang et al. (2025) ------------------------------------

projects <- get_projects()
params <- list(microsynth = list(sc_method = "microsynth", force = NA,
                                 estimator = NA, r = NA, inf_type = NA, progfunc = NA,
                                 covariates = list(c("accessibility", "slope",
                                                     "accessibility_walking_only", "elevation"))))
p_grid <- create_grid("synthetic_controls", params, projects)
p_grid$sc_method <- "pensynth"  # Hacky for now to work with create_grid


# Run penalized synthetic controls across projects
writeLines(paste0(Sys.time(), ": Starting loop"), "cluster_logs/tang_repr_log.txt")
pensynth_res <- foreach(i = 1:nrow(p_grid), .combine = rbind) %dopar% {
  
  params <- p_grid[i, names(p_grid) != "project"]
  proj_names <- sapply(projects, function(p) p[1])
  project <- projects[proj_names == p_grid[i, "project"]][[1]]

  project_name <- project[1]; start_year <- project[2]
  
  # Load the processed dat_long table.
  load(paste0("data/processed/", project_name, "/dat_synth.Rdata"))
  
  # Prep data, formula for synthetic controls.
  keep_vars <- c("ID", "Y", "D", "year", params$covariates[[1]])
  start_year = as.numeric(start_year)
  synth_dat <- as.data.frame(dat_long) %>% 
    mutate(year = as.numeric(year)) %>%
    rename(Y = "cum_loss", D = "treated") %>%
    mutate(Y = Y / 10000) %>%  # Convert cumulative loss from sq. meters to ha
    select(all_of(keep_vars)) %>%
    na.omit()
  
  
  # The "covariates" for pensynth - concatenation of outcome and covariate 
  # time series. Using these from before 2 yrs before treatment, other 2 are
  # for cross-fitting (Z).
  X <- synth_dat %>%
    select(c("ID", "D", "year", "Y", params$covariates[[1]])) %>%
    filter(year < start_year - 2)
  X <- X %>%
    pivot_wider(values_from = 4:ncol(X), names_from = "year")
  
  X1 <- X %>% filter(D == 1) %>% select(-c(ID, D)) %>% as.matrix() %>% t()
  X0 <- X %>% filter(D == 0) %>% select(-c(ID, D)) %>% as.matrix() %>% t()
  
  Z <- synth_dat %>%
    select(c("ID", "D", "year", "Y", params$covariates[[1]])) %>%
    filter(year %in% c(start_year - 2, start_year - 1))
  Z <- Z %>%
    pivot_wider(values_from = 4:ncol(Z), names_from = "year")
  
  Z1 <- Z %>% filter(D == 1) %>% select(-c(ID, D)) %>% as.matrix() %>% t()
  Z0 <- Z %>% filter(D == 0) %>% select(-c(ID, D)) %>% as.matrix() %>% t()
  
  
  # Run pensynth
  res <- cv_pensynth(X1 = X1, X0 = X0, Z1 = Z1, Z0 = Z0,
                     nlambda = 200, opt_pars = clarabel::clarabel_control(),
                     standardize = TRUE, return_solver_info = FALSE,
                     verbose = FALSE, adaptive_lambda = TRUE
  )
  lg <- readLines("cluster_logs/tang_repr_log.txt")
  lg <- c(lg, paste0(Sys.time(), ": Finished point estimate for row ", i))
  writeLines(lg, "cluster_logs/tang_repr_log.txt")
  
  
  Y0 <- synth_dat %>%
    select(c("ID", "D", "year", "Y"))
  Y0 <- Y0 %>%
    pivot_wider(values_from = 4:ncol(Y0), names_from = "year") %>%
    filter(D == 0) %>% select(-c(ID, D)) %>% as.matrix() %>% t()
  
  # sc_out <- (Y0 %*% res$w_opt)
  # y1 <- synth_dat$Y[synth_dat$D == 1]
  # plot(sc_out, type = "l", ylim = c(min(y1, sc_out), max(y1, sc_out)))
  # lines(y1, type = "l", col = "blue")
  # abline(v = start_year, col = "darkgray", lty = 2)
  
  
  # P-value / confidence intervals
  assign("X0", X0, envir = .GlobalEnv)
  assign("X1", X1, envir = .GlobalEnv)  # This must be done to work in parallel
  assign("Z0", Z0, envir = .GlobalEnv)  # parallel because of how the package is 
  assign("Z1", Z1, envir = .GlobalEnv)  # written...
  test <- placebo_test(res, Y1 = synth_dat %>% filter(D == 1) %>% pull(Y),
                       Y0 = Y0)
  lg <- readLines("cluster_logs/tang_repr_log.txt")
  lg <- c(lg, paste0(Sys.time(), ": Finished placebo runs for row ", i))
  writeLines(lg, "cluster_logs/tang_repr_log.txt")
  
  # Pseudo confidence intervals based on placebo distribution at year == 22
  ci <- quantile(test$E0[22, ], c(0.025, 0.975))
  # hist(test$E0[22, ]); abline(v = test$E1[22], col = "red")
  # abline(v = ci, col = "blue")
  
  out <- data.frame(project_name = project_name, year = start_year,
                    ATT = test$E1[22], lower = ci[1], upper = ci[2])
  out <- cbind(out, params[i, ])
  row.names(out) <- NULL
  
  return(out)

}
saveRDS(pensynth_res, "data/results/tang25.rds")

