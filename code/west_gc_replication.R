# Author: Megan Ayers
# Date: 11/3/2025
# Function: Run method replication for Guizar-Coutino et al. (2022) and
#           West et al. (2023).

rm(list = ls())
source("code/setup.R")
projects <- get_projects()


### Guizar-Coutino et al. (2023)
source("code/methods/matching/logic.R")
params <- list(method = "nearest", distance = "mahalanobis", pop.size = NA,
               ratio = 1, covariates = list(c("elevation", "slope",
                                              "accessibility", "defo_distance")),
               std.caliper = TRUE)
params$caliper <- list(rep(0.25, length(params$covariates[[1]])))
names(params$caliper[[1]]) <- params$covariates[[1]]

p_grid <- create_grid("matching", params)

gc22 <- run_sc_method(projects, "matching", p_grid, save_res = FALSE)
saveRDS(gc22, "data/results/gc22.rds")

