# Author: Megan Ayers
# Date: 9/1/2025
# Function: Interactive script for setting projects, methods, and parameters to
#           run for specification curves.

rm(list = ls())
source("code/setup.R")  # Loads packages and grabs functions from other scripts.
registerDoParallel(cores = 5)

projects <- get_projects()[1:3]


### Uncomment / adjust p_list to run for MATCHING.
ate_method <- "matching"
p_list <- list(method = c("nearest", "cem"),
               distance = c("logit", "mahalanobis", "euclidean"),
               ratio = c(1, 3, 5),
               covariates = list(c("treecover_past", "accessibility",
                                   "accessibility_walking_only", "aspect",
                                   "elevation", "slope"),
                                 c("treecover_past", "accessibility")))


### Uncomment / adjust p_list to run for SYNTHETIC CONTROLS.
# ate_method <- "synthetic_controls"
# p_list_g <- list(sc_method = "gsynth",
#                  force = c("none", "unit", "time", "two-way"),
#                  estimator = c("ife"),
#                  r = c(1, 3, 5, "cv"))
# p_list_m <- list(sc_method = "microsynth",
#                  covariates = list(c("treecover_2000", "hillshade", "aspect",
#                                      "elevation", "slope"),
#                                    c("elevation", "slope")))
# p_list_a <- list(sc_method = "augsynth",
#                  inf_type = c("conformal", "jackknife"),
#                  covariates = list(c("elevation", "slope")),
#                  progfunc = c("None", "EN", "Ridge", "RF", "MCP", "CITS",
#                               "seq2seq"),
#                  force = c("none", "two-way"))
# p_list <- list("gysnth" = p_list_g, "microsynth" = p_list_m,
#                "augsynth" = p_list_a)
# # p_list <- list("augsynth" = p_list_a)


make_sc_curves(projects, ate_method, p_list, plot_only = FALSE,
               leftmargin = 5)
