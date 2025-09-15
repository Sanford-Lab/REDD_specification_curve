# Author: Megan, based on earlier code from Albert
# Date: 9/10/2025 (most recent)
# Purpose: Runs synthetic controls logic

library(tidyverse)
library(zoo)
library(tidyquant)
library(gsynth)


### Generalized synthetic controls.
apply_gsynth <- function(project_name, start_year, params,
                         outcome_var = "cum_loss") {
  
  # Load the processed dat_long table.
  load(paste0("data/processed/", project_name, "/dat_synth.Rdata"))
  results_gsynth <- data.frame()
  
  # Handle estimator and inference parameter logic.
  if (params$estimator == "ife") {
    this_inference = "parametric"
  } else if (params$estimator == "mc") {
    this_inference = "nonparametric"
  }
  
  # Prep data for generalized synthetic controls.    
  start_year = as.numeric(start_year)
  synth_dat <- as.data.frame(dat_long) %>% 
    mutate(year = as.numeric(year),
           D = ifelse(treated == 1 & year >= start_year, 1, 0)) %>%
    rename(Y = outcome_var) %>%
    select(all_of(c("ID", "Y", "D", "year", params$covariates[[1]])))
  
  if (params$r == "cv") {
    params$r <- 1
    CV <- TRUE
  } else {
    params$r <- as.numeric(params$r)
    CV <- FALSE
  }
  
  # Run generalized synthetic controls and standardize output for spec curves.
  if (!is.null(params$covariates)) {
    form <- as.formula(paste("Y ~ D +", paste(params$covariates[[1]],
                                            collapse = " + ")))
  } else {
    form <- as.formula("Y ~ D")
  }
  
  out_gsynth <- gsynth(form, data = synth_dat, index = c("ID", "year"), 
                       force = params$force, estimator = params$estimator,
                       r = params$r, CV = CV, se = TRUE, nboots = 500,
                       inference = this_inference, parallel = FALSE,
                       seed = 0930)
  
  these_results = out_gsynth$est.att %>%
    as.data.frame() %>%
    rownames_to_column(var = "year") %>%
    mutate(year = as.numeric(year) + start_year - 1)  # Un-center year
  
  these_results <- these_results %>%
    rename(se = "S.E.",
           coef = "ATT",
           lower = "CI.lower",
           upper = "CI.upper") %>%
    select(all_of(c("year", "coef", "lower", "upper")))
  
  return(these_results)
  
}


### Wrapper logic function.
execute_method <- function(project_name, start_year, params) {
  
  if (params$sc_method == "gsynth") {
    res <- apply_gsynth(project_name, start_year, params)
  }
  
  return(res)
  
} 


