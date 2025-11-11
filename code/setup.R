# Author: Megan Ayers
# Date: 10/11/2025
# Function: Load (or install) all packages and run function scripts.

# Install augsynth, which isn't on CRAN:
if (!"augsynth" %in% installed.packages()) {
  devtools::install_github("ebenmichael/augsynth") 
}

# Install MCPanel, which isn't on CRAN:
# (NOTE: On the cluster, follow code/cluster_init/instructions.md to install
#  MCPanel before this point. This should work on local machines, though.)
if (!"augsynth" %in% installed.packages()) {
  devtools::install_github("susanathey/MCPanel", force=TRUE)
}

# For installing gsynth properly given this version of R:
if (!"gsynth" %in% installed.packages()) {
  remotes::install_version("ggplot2", version = "3.4.4")
  remotes::install_version("GGally", version = "2.1.2")
  install.packages("gsynth")
}

packages <- c("augsynth", "doParallel", "estimatr", "ggplot2", "glmnet", "keras3",
              "grf", "gsynth", "Matching", "MatchIt", "MCPanel", "microsynth",
              "randomForest", "rgenoud", "sf", "stringr", "tidyr", "tidyverse", 
              "tidyquant", "zoo", "dplyr")

# Install any missing packages.
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

# Load all
invisible(lapply(packages, library, character.only = TRUE))
select <- dplyr::select

# Load helper functions
source("code/project_curves_wrapper.R")
source("code/projects/universal_list_of_projects.R")

