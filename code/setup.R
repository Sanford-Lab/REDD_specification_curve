# Author: Megan Ayers
# Date: 10/11/2025
# Function: Load (or install) all packages and run function scripts.

# Install augsynth, which isn't on CRAN:
if (!"augsynth" %in% installed.packages()) {
  devtools::install_github("ebenmichael/augsynth") 
}

# For installing gsynth properly given this version of R:
if (!"gsynth" %in% installed.packages()) {
  remotes::install_version("ggplot2", version = "3.4.4")
  remotes::install_version("GGally", version = "2.1.2")
  install.packages("gsynth")
}

packages <- c("augsynth", "here", "doParallel", "estimatr", "ggplot2", "glmnet",
              "grf", "gsynth", "MatchIt", "MCPanel", "microsynth",
              "randomForest", "sf", "stringr", "tidyr", "tidyverse", 
              "tidyquant", "zoo", "dplyr")

# Install any missing packages.
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

# Load all
invisible(lapply(packages, library, character.only = TRUE))

# Load helper functions
source(here("code", "project_curves_wrapper.R"))
source(here('code', 'Projects', 'universal_list_of_projects.R'))

