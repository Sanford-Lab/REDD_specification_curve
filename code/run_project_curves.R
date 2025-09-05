# Author: Megan Ayers
# Date: 9/1/2025
# Function: Interactive script for setting projects, methods, and parameters to
#           run for specification curves.

rm(list = ls())
library(here)
source(here("code", "project_curves_wrapper.R"))
source(here('code', 'Projects', 'universal_list_of_projects.R'))

projects <- get_projects()
ate_method <- "matching"
p_list <- list(method = c("nearest", "cem"),
               distance = c("logit", "mahalanobis", "euclidean"),
               ratio = c(1, 3, 5),
               covariates = list(c("treecover_past", "accessibility",
                                   "accessibility_walking_only", "aspect",
                                   "elevation", "slope"),
                                 c("treecover_past", "accessibility")))

make_sc_curves(projects, ate_method, p_list)
