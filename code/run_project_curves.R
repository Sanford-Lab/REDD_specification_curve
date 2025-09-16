# Author: Megan Ayers
# Date: 9/1/2025
# Function: Interactive script for setting projects, methods, and parameters to
#           run for specification curves.

rm(list = ls())
library(here)
source(here("code", "project_curves_wrapper.R"))
source(here('code', 'Projects', 'universal_list_of_projects.R'))

projects <- get_projects()

### Uncomment / adjust p_list to run for MATCHING.
# ate_method <- "matching"
# p_list <- list(method = c("nearest", "cem"),
#                distance = c("logit", "mahalanobis", "euclidean"),
#                ratio = c(1, 3, 5),
#                covariates = list(c("treecover_past", "accessibility",
#                                    "accessibility_walking_only", "aspect",
#                                    "elevation", "slope"),
#                                  c("treecover_past", "accessibility")))


### Uncomment / adjust p_list to run for SYNTHETIC CONTROLS.
ate_method <- "synthetic_controls"
p_list_g <- list(sc_method = c("gsynth"),
                 force = c("none", "unit", "time", "two-way"),
                 estimator = c("ife"),
                 r = c(1, 3, 5, "cv"))
p_list_m <- list(sc_method = c("microsynth"),
                 covariates = list(c("treecover_2000", "hillshade", "aspect",
                                     "elevation", "slope"),
                                   c("elevation", "slope")))
p_list <- list("gysnth" = p_list_g, "microsynth" = p_list_m)


make_sc_curves(projects, ate_method, p_list, plot_only = FALSE,
               leftmargin = 5)
