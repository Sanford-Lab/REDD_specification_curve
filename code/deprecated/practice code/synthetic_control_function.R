# Function: Function to apply synthetic control parameters include start year of project and project name
# Author: Rachel Bitutsky
# Date: 7/25/2023

# Load required libraries
library(microsynth)
library(Synth)
library(tidyverse)
library(zoo)
library(tidyquant)

# Define the function for performing Synthetic Controls for a specific project
synthetic_control_data <- function(project_name, start_yr){ 
  
  # Load the dataset for the specific project
  load(paste0("data/processed/", project_name, "/dat_synth.Rdata"))
  
  dat_long <- as.data.frame(dat_long) %>% 
    mutate(year = as.numeric(year),
           D = ifelse(treated ==1 & year >= first_treated_year, 1, 0),
           Y = loss)
  
  my_predictors <- c("accessibility", "treecover_2000", "elevation", "slope")
  
  
  match.out = "Y"
  
  out <- microsynth(dat_long, 
                     idvar="ID", timevar="year", intvar="treated", 
                     start.pre=1, end.pre=start_yr, end.post=22, 
                     match.out=match.out, match.covar=my_predictors, 
                     result.var=match.out, 
                     test="lower",
                     perm=250, jack=TRUE
                     , check.feas=TRUE, use.backup=TRUE
  )
  
  plot <- plot_microsynth(out)
  print(out)
  
  return(out, plot)
  
  
  # # Get treated and control units
  # treated_unit <- dat_long %>% filter(treated == 1) %>% pull(ID) %>% unique()
  # control_units <- dat_long %>% filter(treated == 0) %>% pull(ID) %>% unique()
  # control_units <- control_units[1:20]  # You can change this based on your needs
  # 
  # # Perform data preparation using dataprep function
  # dataprep.out <-
  #   dataprep(foo = dat_long,
  #            predictors = c("accessibility", "treecover_2000", "elevation", "slope"),
  #            dependent = "treecover_remaining",
  #            unit.variable = "ID",
  #            time.variable = "year",
  #            treatment.identifier = treated_unit,
  #            controls.identifier = control_units,
  #            time.predictors.prior = c(1:(start_yr-1)),   # Use the provided time_pred_prior argument
  #            time.optimize.ssr = c(1:start_yr),         # Use the provided time_opt_ssr argument
  #            time.plot = c(1:22)
  #   )
  # 
  # # Perform synth analysis using the created dataprep object
  # synth_out <- synth(data.prep.obj = dataprep.out)
  
  # Plot the path
  # path.plot(synth_out, dataprep.out, Ylab = 'Treecover Remaining')
  
}

# Call the function with the specific project name and optional arguments
# Example usage:
synth_agrocortex <- synthetic_control_data("agrocortex", start_yr = 9)
synth_agrocortex$plot
summary(synth_agrocortex$out)

# synthetic_control_data("other_project", time_pred_prior = c(1:10), time_opt_ssr = c(1:11))