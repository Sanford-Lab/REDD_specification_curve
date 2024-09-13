# Rachel - Agrocortex Synthetic Controls

# Modified by Albert: 10/03/2023

library(Synth)
library(tidyverse)
library(zoo)
library(tidyquant)
library(tidysynth)
library(gsynth)
data(gsynth)
# I assume this is the dataframe we use
load("data/processed/agrocortex/dat_synth.Rdata")

first_treated_year = 9

dat_long <- as.data.frame(dat_long) %>% 
  mutate(year = as.numeric(year),
         D = ifelse(treated ==1 & year >= first_treated_year, 1, 0),
         Y = treecover_remaining)

# look at the data
dat_long %>% glimpse()
class(dat_long)

panelview(Y ~ treated, data = dat_long,  index = c("ID","year"), pre.post = TRUE, display.all = T, type = "outcome")

out <- gsynth(Y ~ D
               + treecover_2000 
              , data = dat_long, 
              index = c("ID","year"), 
              force = "two-way", 
              CV = TRUE, r = c(0, 5), se = TRUE, 
              inference = "parametric", nboots = 1000, 
              parallel = FALSE)

print(out)
out$est.att
out$est.avg
out$est.beta


cumu1 <- cumuEff(out, cumu = TRUE, id = NULL, period = c(0,5))
cumu1$est.catt

plot(out)

plot(out, type = "counterfactual", raw = "none", main="")
























treated_unit <- dat_long %>% filter(treated == 1) %>% pull(ID) %>% unique()
class(treated_unit)

out <- dat_long %>%
  # initial the synthetic control object
  synthetic_control(outcome = loss, # outcome
                    unit = ID, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = treated_unit, # unit where the intervention occurred
                    i_time = first_treated_year, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(time_window = min(dat_long$year):first_treated_year,
                     treecover_2000 = mean(treecover_2000, na.rm = T),
                     accessibility = mean(accessibility, na.rm = T),
                     elevation = mean(elevation, na.rm = T),
                     slope = mean(slope, na.rm = T)
                     ) %>%
  # generate_predictor(time_window = 1988,
  #                    cigsale_1988 = cigsale) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = min(dat_long$year):(first_treated_year-1), # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()


out %>% plot_trends()


out %>% plot_differences()

out %>% plot_weights()


out %>% plot_placebos()

out %>% grab_significance()





# Previous code from Rachel
# ---------------------------------------------------------------------------


treated_unit <- dat_long %>% filter(treated == 1) %>% pull(ID) %>% unique()
control_units <- dat_long %>% filter(treated != 1) %>% pull(ID) %>% unique()
# treatment variable is called "treated"
# time variable is year
# outcome variable is loss
dataprep.out <-
  dataprep(foo = dat_long,
           predictors = c("accessibility"
          # , "treecover_2000",
          # "elevation",
          # "slope"
          ),
           dependent     = "loss",
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

gaps.plot(synth_out, dataprep.out, tr.intake = 9)
