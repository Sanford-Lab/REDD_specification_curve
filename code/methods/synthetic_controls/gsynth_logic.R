# Author: Albert (3/4/2024)
# Date: 3/4/2024 (most recent)
# Purpose: Runs synthetic controls logic

library(tidyverse)
library(zoo)
library(tidyquant)
library(gsynth)


# NOTE: need to add function parameters for all the different things you can tweak for 
# synthetic controls (i.e. all the different possible combinations of running it)
apply_gsynth <- function(project_name, start_year, outcome_var = "cum_loss", covariates = NULL, force_options = c("none", "unit", "time", "two-way"), estimators = c("ife", "mc")) {
  
  # load the dat_long table
  load(paste0("data/processed/", project_name, "/dat_synth.Rdata"))
  results_gsynth <- data.frame()
  
  my_predictors = covariates
  
  for(f in force_options){
    for(e in estimators){
      
      if(e == "ife"){
        this_estimator = "interactive FE"
        this_inference = "parametric"
      } else{
        this_estimator = "matrix completion"
        this_inference = "nonparametric"
      }
      
      start_year = as.numeric(start_year)
      
      synth_dat <- as.data.frame(dat_long) %>% 
        mutate(year = as.numeric(year),
               D = ifelse(treated ==1 & year >= start_year, 1, 0))%>%
        rename(Y = outcome_var)%>%
        select(ID, Y, D, year, my_predictors)
      
      out_gsynth <- gsynth(Y ~ D
                           , data = synth_dat, 
                           index = c("ID","year"), 
                           force = f, 
                           estimator = e,
                           CV = TRUE, 
                           se = TRUE, nboots = 500, inference = this_inference, 
                           parallel = FALSE,
                           seed = 0930
                           )
      
      plot(out_gsynth)
      
      if (outcome_var == "loss"){
        these_results = out_gsynth$est.avg
      } else{
        these_results = out_gsynth$est.att %>%
          as.data.frame()%>%
          rownames_to_column(var = "year")%>%
          filter(year == max(as.numeric(year)))
      }
      
      
      these_results <- these_results %>%
        rename(se = "S.E.")%>%
        select(ATT, se)%>%
        mutate(project = project_name,
               "fixed effects" = f,
               "estimator" = this_estimator,
               "synth_method" = "generalized SC")
      
      results_gsynth <- rbind(results_gsynth, these_results)
      
    }
    
    
  }
  
  
  return(results_gsynth)
  
  
  
}


