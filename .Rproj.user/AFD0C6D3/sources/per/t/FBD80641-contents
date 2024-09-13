"-----------------------------------------------------------------------------------"
"Note: Example of the standard generalized synthetic control analyses for Peru      "
"      Modifications to the code might be required for different projects/countries "
"-----------------------------------------------------------------------------------"

library(gsynth)
library(Rcpp)
library(panelView)
library(cowplot)

#PART 1-----------------------------------------------------------------------------------
setwd("...")
controls.used = list.files(pattern="*controls_used.csv") #select raster names for the loop

control.data <- read.csv(controls.used[1]) 
control.data$ID <-  as.character(control.data$unit.names)
control.data$unit.names <- as.character(control.data$unit.names)
control.data$project <- controls.used[1]

for (i in 2:length(controls.used)) {
  temp <- read.csv(controls.used[i])
  temp$ID <-  as.character(temp$unit.names)
  temp$unit.names <- as.character(temp$unit.names)
  temp$project <- controls.used[i]
  control.data <- rbind(control.data, temp)
  rm(temp)
  print(controls.used[i])
}

projects <- control.data$project

names <- t(as.data.frame(strsplit(control.data$project,"_"))[1,])
control.data$project <- names[,1]
rm(names)
controls.used <- as.numeric(unique(control.data$unit.names))

unique(control.data$project)
control.data$project <- as.integer(control.data$project)
control.data$ID <- as.integer(control.data$ID)

#matching controls used in the SCs with their deforestation information:
redd.data <- read.csv("Peru_synth_control_data.csv")[,-1]
redd.data$project <- redd.data$ID
redd.data$ID <- redd.data$polygon_ID #ID refers to polygon_ID!

data.cf <- subset(redd.data, ID == controls.used[1]) 
for (i in 2:length(controls.used)) {
  temp <- subset(redd.data, ID == controls.used[i])
  data.cf <- rbind(data.cf, temp)
  rm(temp)
}
rm(redd.data)

data.cf[0,]
data.cf <- data.cf[,c("project","ID","REDD","year","polygon_ID","polygon_ha","def","mm","buf1k_def","buf10k_def")]

#data preparation-------------------------------------------------------------
setwd("...")
d <- read.csv("Peru_synth_control_data.csv")[,-1]
d[0,]
d$project <- NA
d <- d[,c("project","ID","REDD","year","polygon_ID","polygon_ha","def","mm","buf1k_def","buf10k_def")]
d <- subset(d, REDD==1)
table(d$REDD)
unique(d$ID) #projects

d <- rbind(d, data.cf)
rm(data.cf)

#polygon_ID selected with MatchIt
controls_list <- unique(d$polygon_ID[d$REDD==0])
MatchIt <- read.csv("...")[,2]
length(setdiff(controls_list, MatchIt))
length(setdiff(MatchIt, controls_list))


startdate <- data.frame(project =   c(1182, 13601,  13602,  13603,  2278,  1067,   985,   958,   944,   844),
                        startdate = c(2013,  2010,   2010,   2010,  2018,  2011,  2009,  2011,  2009,  2009))

d$treat <- 0
d$treat[ d$ID==1182 & d$year >= 2013 ] <- 1 
d$treat[ d$ID==13601 & d$year >= 2010 ] <- 1 
d$treat[ d$ID==13602 & d$year >= 2010 ] <- 1 
d$treat[ d$ID==13603 & d$year >= 2010 ] <- 1 
d$treat[ d$ID==2278 & d$year >= 2018 ] <- 1 
d$treat[ d$ID==1067 & d$year >= 2011 ] <- 1 
d$treat[ d$ID==985 & d$year >= 2009 ] <- 1 
d$treat[ d$ID==958 & d$year >= 2011 ] <- 1 
d$treat[ d$ID==944 & d$year >= 2009 ] <- 1 
d$treat[ d$ID==844 & d$year >= 2009 ] <- 1 

d$def_p <- d$def/d$polygon_ha * 100#proportional deforestation
d$buf1k_def_p <- d$buf1k_def/d$polygon_ha * 100
d$buf10k_def_p <- d$buf10k_def/d$polygon_ha * 100

unique(d$project)

set.seed(131313)
out <- gsynth(def_p ~ treat + mm + buf1k_def + buf10k_def,
              data = d,
              #data = subset(d, ID !=985 & ID != 13601),
              index = c("polygon_ID","year"),
              force = "two-way",
              EM = TRUE, #s = 500,
              min.T0 = 5,
              CV = TRUE, r = c(0, 5),
              #lambda = 10,
              estimator = "mc",
              se = TRUE, nboots = 1000)
out
out$est.att
write.csv(round(out$est.att, digits = 4), "Peru_GSC_ATT.csv")
out$est.avg
p3 <- plot(out, xlab = "", xlim = c(-25,11), ylim = c(-2.1,1),
           ylab = "ATT on deforestation (%)", main = "Peru", theme.bw = F)
p4 <- plot(out, type = "counterfactual", raw = "band", xlab = " ",
           xlim = c(-25,11), ylim = c(-0.1,2),
           ylab = "Deforestation (%)", main = "", theme.bw = F, shade.post = FALSE,
           legendOff = T)

g2 <- plot_grid(p3, p4, align = "v", nrow = 2, rel_heights = c(0.55, 0.5))
g2
ggsave(g2, units="cm", width=21, height=16.5, file="Peru_GSC.svg")
ggsave(g2, dpi=600, units="cm", width=21, height=16.5, file="Peru_GSC.png")

