# ADPML Gsynth

library(gsynth)
load("data/processed/adpml/dat_synth.Rdata")

library(panelView)
panelview(treecover_remaining ~ treated, data = dat_long, index = c("ID", "year"), pre.post = TRUE)

# OPTION 1_______________________________________

synth_obj <- gsynth(treatment = 'treated',
                    time_var = 'year',
                    group_var = 'ID',
                    data = dat_long,
                    Y = 'treecover_remaining',
                    X = ~ 'accessibility' + 'treecover_2000' + 'elevation' + 'slope') 

# OPTION 2____________________________________


gsynth.out <- gsynth(
  loss ~ treated + accessibility+ treecover_2000 + elevation + slope,
  data = dat_long,
  index = c("ID", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  nboots = 1000,
  parallel = F # TRUE
)

# OPTION 3___________________________________

synth_obj <- gsynth(
  Y = dat_long$treecover_remaining,
  X = ~ accessibility + treecover_2000 + elevation + slope,
  Z = dat_long$treated_unit,
  T = dat_long$year,
  unit.variable = "ID",
  control.identifier = control_units,
  time.predictors.prior = 1:8,
  time.optimize.ssr = 1:9,
  time.plot = 1:22
)
__________________________________

# OPTION 4

system.time(
  out <- gsynth(treecover_remaining ~ treated + accessibility + treecover_2000 + elevation + slope, data = dat_long, 
                index = c("ID","year"), force = "two-way", 
                CV = TRUE, r = c(0, 5), se = TRUE, 
                inference = "parametric", nboots = 1000, 
                parallel = FALSE)
)

plot(gsynth.out)

synth_out <- synth(data.prep.obj = dataprep.out)

path.plot(synth_out, dataprep.out,Ylab = 'Treecover Remaining')

gaps.plot(synth_out, dataprep.out)
