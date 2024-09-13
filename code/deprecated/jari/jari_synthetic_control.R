# Rachel - Synthetic Control for Jari

library(Synth)
library(tidyverse)
library(zoo)
library(tidyquant)

# I assume this is the dataframe we use
load("data/processed/jari/dat_synth.Rdata")

# look at the data
head(dat_long)

dat_long <- as.data.frame(dat_long) %>% 
  mutate(year = as.numeric(year))

class(dat_long)

(treated_unit <- dat_long %>% filter(treated == 1) %>% pull(ID) %>% unique())
(control_units <- dat_long %>% filter(treated == 0) %>% pull(ID) %>% unique())
control_units <- control_units[1:200]

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