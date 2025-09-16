# Author: Megan, based on earlier code from Albert, Henry, Rachel
# Date: 9/10/2025 (most recent)
# Purpose: Runs synthetic controls logic

library(tidyverse)
library(zoo)
library(tidyquant)
library(gsynth)
library(microsynth)


execute_method <- function(project_name, start_year, params,
                           outcome_var = "cum_loss") {
  
  # Load the processed dat_long table.
  load(paste0("data/processed/", project_name, "/dat_synth.Rdata"))
  
  # Prep data, formula for synthetic controls.    
  start_year = as.numeric(start_year)
  synth_dat <- as.data.frame(dat_long) %>% 
    mutate(year = as.numeric(year),
           D = ifelse(treated == 1 & year >= start_year, 1, 0)) %>%
    rename(Y = outcome_var) %>%
    select(all_of(c("ID", "Y", "D", "year", params$covariates[[1]])))
  
  if (!is.null(params$covariates[[1]])) {
    form <- as.formula(paste("Y ~ D +", paste(params$covariates[[1]],
                                              collapse = " + ")))
  } else {
    form <- as.formula("Y ~ D")
  }
  
  
  ### Generalized synthetic controls logic.
  if (params$sc_method == "gsynth") {
    
    # Handle estimator and inference parameter logic.
    if (params$estimator == "ife") {
      this_inference = "parametric"
    } else if (params$estimator == "mc") {
      this_inference = "nonparametric"
    }
    
    if (params$r == "cv") {
      params$r <- 1; CV <- TRUE
    } else {
      params$r <- as.numeric(params$r); CV <- FALSE
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
    
    
  ### Micro synthetic controls logic 
  } else if (params$sc_method == "microsynth") {
    
    out_microsynth <- microsynth(as.data.frame(synth_dat), 
                                 idvar = "ID", timevar = "year", intvar = "D", 
                                 start.pre = 1,  end.pre = (start_year - 1), 
                                 end.post = start_year:22,  match.out = "Y",
                                 match.covar.min = params$covariates[[1]], 
                                 result.var = "Y",  test = "two-sided",
                                 perm = 250, jack = FALSE, check.feas = TRUE,
                                 use.backup = TRUE, use.survey = FALSE)
    
    these_results <- as.data.frame(do.call(rbind, out_microsynth$Results))
    rownames(these_results) <- NULL
    
    these_results <- these_results %>%
      mutate(year = start_year:22,
             coef = Trt - Con,
             se = abs(coef/qnorm(Perm.pVal)),
             lower = coef - 1.96*se,
             upper = coef + 1.96*se) %>%
      select(year, coef, lower, upper)
    
  }
  
  
  return(these_results)
  
}

