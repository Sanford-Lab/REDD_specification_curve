# GSynth Practice Tutorial

library(gsynth)
data(gsynth)
ls()

head(simdata)

install.packages("panelView")
library(panelView)

panelview(Y ~ D, data = simdata, index = c("id", "time"), pre.post = TRUE)

system.time(
  out <- gsynth(Y ~ D + X1 + X2, data = simdata, 
                index = c("id","time"), force = "two-way", 
                CV = TRUE, r = c(0, 5), se = TRUE, 
                inference = "parametric", nboots = 1000, 
                parallel = FALSE)
)

print(out)
