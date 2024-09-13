# Rachel - Florestal Santa Maria Synthetic Controls

library(Synth)
library(tidyverse)
library(zoo)
library(tidyquant)

# I assume this is the dataframe we use
load("data/processed/florestal/dat_synth.Rdata")

# look at the data
head(dat_long)

dat_long <- as.data.frame(dat_long) %>% 
  mutate(year = as.numeric(year))

class(dat_long)

(treated_unit <- dat_long %>% filter(treated == 1) %>% pull(ID) %>% unique())
(control_units <- dat_long %>% filter(treated == 0) %>% pull(ID) %>% unique())
control_units <- control_units[1:50]

# treatment variable is called "treated"
# time variable is year
# outcome variable is loss
dataprep.out <-
  dataprep(foo = dat_long,
           predictors = c("accessibility","treecover_2000","elevation","slope"),
           dependent     = "treecover_remaining",
           unit.variable = "ID",
           time.variable = "year",
           # unit.names.variable = "system:index",
           treatment.identifier  = treated_unit,
           controls.identifier   = control_units,
           time.predictors.prior = c(1:8),
           time.optimize.ssr     = c(1:9),
           time.plot             = c(1:22)
  )

synth_out <- synth(data.prep.obj = dataprep.out)

path.plot(synth_out, dataprep.out,Ylab = 'Treecover Remaining')

gaps.plot(synth_out, dataprep.out)


### Try with tidySynth

library(tidysynth)

data("smoking")

florestal_out <-
  
  dat_long %>%
  
  # initial the synthetic control object
  synthetic_control(outcome = treecover_remaining, # outcome
                    unit = ID, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = treated_unit, # unit where the intervention occurred
                    i_time = 9, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1:8,
                     accessibility = mean(accessibility, na.rm = T),
                     treecover_2008 = mean(treecover_2000, na.rm = T),
                     elevation = mean(elevation, na.rm = T),
                     slope = mean(slope, na.rm=T)) %>%
  
  
  # Lagged cigarette sales 
  generate_predictor(time_window = 1,
                     treecover_remaining_01 = treecover_remaining) %>%
  generate_predictor(time_window = 4,
                     treecover_remaining_04 = treecover_remaining) %>%
  generate_predictor(time_window = 8,
                     treecover_remaining_8 = treecover_remaining) %>%
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1:8, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

florestal_out %>% plot_differences()
