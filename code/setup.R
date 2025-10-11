# Author: Megan Ayers
# Date: 10/11/2025
# Function: Load all packages and run function scripts.

# devtools::install_github("ebenmichael/augsynth")
# install_github("susanathey/MCPanel")
library(augsynth)
library(here)
library(doParallel)
library(estimatr)
library(ggplot2)
library(glmnet)
library(grf)
library(gsynth)
library(MatchIt)
library(MCPanel)
library(microsynth)
library(randomForest)
library(sf)
library(stringr)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(zoo)
library(dplyr)


source(here("code", "project_curves_wrapper.R"))
source(here('code', 'Projects', 'universal_list_of_projects.R'))


