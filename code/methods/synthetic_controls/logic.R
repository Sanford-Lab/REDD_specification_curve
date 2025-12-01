# Author: Megan, based on earlier code from Albert, Henry, Rachel
# Date: 9/10/2025 (most recent)
# Purpose: Runs synthetic controls logic

if (interactive()) {
  library(tidyverse)
  library(zoo)
  library(tidyquant)
  library(gsynth)
  library(microsynth)
  # devtools::install_github("ebenmichael/augsynth")
  # install_github("susanathey/MCPanel")
  library(augsynth)
  library(randomForest)
  library(glmnet)
  library(MCPanel)
}

execute_method <- function(project_name, start_year, params, n_cores = 1,
                           outcome_var = "cum_loss") {
  
  # Load the processed dat_long table.
  load(paste0("data/processed/", project_name, "/dat_synth.Rdata"))
  
  # Prep data, formula for synthetic controls.
  keep_vars <- c("ID", "Y", "D", "year", params$covariates[[1]])
  start_year = as.numeric(start_year)
  synth_dat <- as.data.frame(dat_long) %>% 
    mutate(year = as.numeric(year),
           D = ifelse(treated == 1 & year >= start_year, 1, 0)) %>%
    rename(Y = outcome_var) %>%
    select(all_of(keep_vars))
  
  # Convert cumulative loss from sq. meters to hectares
  if (outcome_var == "cum_loss") {
    synth_dat$Y <- synth_dat$Y / 10000
  }
  
  if (!is.null(params$covariates[[1]])) {
    sep <- if (params$sc_method == "augsynth") "|" else "+"
    form <- as.formula(paste("Y ~ D", sep, paste(params$covariates[[1]],
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
                         inference = this_inference, parallel = TRUE,
                         seed = 0930, cores = n_cores)
    
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
    
    synth_dat <- synth_dat %>%
      drop_na(all_of(params$covariates[[1]]))
    if (sum(synth_dat$D == 1) == 0) {
      stop("No treated pixels remain after removing cases with missing covariates.")
    }
    
    out_microsynth <- microsynth(as.data.frame(synth_dat), 
                                 idvar = "ID", timevar = "year", intvar = "D", 
                                 start.pre = 1,  end.pre = (start_year - 1), 
                                 end.post = start_year:22,  match.out = "Y",
                                 match.covar.min = params$covariates[[1]],
                                 omnibus.var = NULL,
                                 result.var = "Y",  test = "two-sided",
                                 perm = 250, jack = FALSE, check.feas = TRUE,
                                 use.backup = TRUE, n.cores = n_cores)
    
    these_results <- as.data.frame(do.call(rbind, out_microsynth$Results))
    rownames(these_results) <- NULL
    
    these_results <- these_results %>%
      mutate(year = start_year:22,
             coef = Trt - Con,
             lower = Linear.Lower * Con,
             upper = Linear.Upper * Con) %>%
      select(year, coef, lower, upper)
    
  ### Augmented synthetic controls logic 
  } else if (params$sc_method == "augsynth") {
    
    # Drop cases with missing covariate values.
    synth_dat <- synth_dat %>%
      drop_na(all_of(params$covariates[[1]]))
    if (sum(synth_dat$D == 1) == 0) {
      stop("No treated pixels remain after removing cases with missing covariates.")
    }
    
    out_augsynth <- augsynth(form, unit = ID, time = year, data = synth_dat,
                             progfunc = params$progfunc, scm = TRUE)
    sum_augsynth <- summary(out_augsynth,  # This takes a while! 
                            inf = TRUE, inf_type = params$inf_type)
    if (ncol(sum_augsynth$att) > 3) {  # CIs returned depending on progfunc
      these_results <- sum_augsynth$att[, 1:4]
      names(these_results) <- c("year", "coef", "lower", "upper")
    } else {
      these_results <- sum_augsynth$att[, 1:3]
      names(these_results) <- c("year", "coef", "se")
      these_results$lower <- these_results$coef - 1.96 * these_results$se
      these_results$upper <- these_results$coef + 1.96 * these_results$se
      these_results <- these_results[, c("year", "coef", "lower", "upper")]
    }
      
  } else {
    stop("Set params$sc_method to either: gsynth, microsynth, or augsynth.")
  }
  
  return(these_results)
  
}

