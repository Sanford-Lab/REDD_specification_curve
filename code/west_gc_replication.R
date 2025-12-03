# Author: Megan Ayers
# Date: 11/3/2025
# Function: Run method replication for Guizar-Coutino et al. (2022) and
#           West et al. (2023).

rm(list = ls())
source("code/setup.R")
projects <- get_projects()


### Guizar-Coutino et al. (2022)
# source("code/methods/matching/logic.R")
# params <- list(method = "nearest", distance = "mahalanobis", pop.size = NA,
#                ratio = 1, covariates = list(c("elevation", "slope",
#                                               "accessibility", "defo_distance")),
#                std.caliper = TRUE)
# params$caliper <- list(rep(0.25, length(params$covariates[[1]])))
# names(params$caliper[[1]]) <- params$covariates[[1]]
# 
# p_grid <- create_grid("matching", params)
# 
# gc22 <- run_sc_method(projects, "matching", p_grid, save_res = FALSE)
# # saveRDS(gc22, "data/results/gc22.rds")


### West et al. (2023)
### TODO: **** Update covariates once more are pulled from GEE. Refer to 
###            West et al. (2023) supplementary materials, Table S2. ***
source("code/methods/synthetic_controls/logic.R")
params <- list(microsynth = list(sc_method = "microsynth", force = NA,
                                 estimator = NA, r = NA, inf_type = NA, progfunc = NA,
                                 covariates = list(c("accessibility", "elevation", "slope"))))
p_grid <- create_grid("synthetic_controls", params, projects)

west23 <- run_sc_method(projects, "synthetic_controls", p_grid, save_res = FALSE)
saveRDS(west23, "data/results/west23.rds")

