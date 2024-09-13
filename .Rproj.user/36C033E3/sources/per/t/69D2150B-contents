"--------------------------------------------------------------------------------------------------"
"Note: Example of the standard generalized synthetic control analyses for with cardinality matching"
"--------------------------------------------------------------------------------------------------"

"###########################################################################################"
 #### Generalized Synthetic Control with MatchIt: Colombia ####
"###########################################################################################"
library(gsynth)
library(Rcpp)
library(panelView)
library(cowplot)
library(plyr)
library(ggplot2)
library(MatchIt)
library(cobalt)
library(gridExtra)

setwd("West et al. (2023) data/Main datasets")
d <- read.csv("Colombia_synth_control_data.csv")[,-1]
d[0,]
d$project <- NA
table(d$REDD)
unique(d$ID) #projects

startdate <- data.frame(project =   c(1389,  1390,  1391,  1392,  1395,  1396,  1400,  1566,  856),
                        startdate = c(2013,  2014,  2013,  2013,  2013,  2014,  2013,  2013,  2011))

d$treat <- 0
d$treat[ d$ID==1389 & d$year >= 2013 ] <- 1 
d$treat[ d$ID==1390 & d$year >= 2014 ] <- 1 
d$treat[ d$ID==1391 & d$year >= 2013 ] <- 1 
d$treat[ d$ID==1392 & d$year >= 2013 ] <- 1 
d$treat[ d$ID==1395 & d$year >= 2013 ] <- 1 
d$treat[ d$ID==1396 & d$year >= 2014 ] <- 1 
d$treat[ d$ID==1400 & d$year >= 2013 ] <- 1 
d$treat[ d$ID==1566 & d$year >= 2013 ] <- 1 
d$treat[ d$ID==856  & d$year >= 2011 ] <- 1  

redd.data <- d

d <- subset(d, year <= 2011)

d <- subset(d, treat==0)
d <- ddply(.data=d, .(project, polygon_ID), .fun=summarise,
           def = mean(def),
           REDD = mean(REDD),
           polygon_ha = mean(polygon_ha),
           pa = mean(pa),
           tree = mean(tree),
           dem = mean(dem),
           slope = mean(slope),
           fric = mean(fric),
           mm = mean(mm),
           buf1k_def = mean(buf1k_def),
           buf10k_def = mean(buf10k_def),
           IL = mean(IL),
           mine = mean(mine),
           palm = mean(palm))

colnames(d) <- c('Project','polygon_ID','Deforestation','REDD','Size',
                 'Protected_cover','Tree_cover','Elevation','Slope',
                 'Travel_distance','Precipitation','Buffer_1km','Buffer_10km',
                 "Indigenous_land","Palm_oil")

#run matching-------------------------------------------------------------------------------
m.out <- matchit(REDD ~ Deforestation + Protected_cover + Tree_cover + Elevation + Slope +
                   Travel_distance + Precipitation + Buffer_1km + Buffer_10km, # + Indigenous_land + Palm_oil,
                 data = d, method = "cardinal", ratio = 10, tols = 0.1)

summary_colombia <- summary(m.out, un = FALSE)

m.out1 <- m.out
cb1 <- love.plot(m.out1, abs = TRUE, binary = "std", thresholds = c(m = .1),
                 sample.names = c("Unmatched", "Matched"),
                 limits = c(0, 3),
                 position = "top",
                 shapes = c("circle", "triangle"),
                 colors = c("red", "blue")) + ggtitle("B. Covariate balance: Colombian REDD+ sites")
cb1

#export matched data
m.data <- match.data(m.out, data = d)
m.data$keep <- 1

d <- redd.data #bring in the full data

projects_main <- subset(d, REDD==1) #need to exclude project that was not matched 
unique(projects_main$polygon_ID)
controls_main <- subset(d, REDD==0)
controls_main$keep <- m.data[match(with(controls_main, polygon_ID), with(m.data, polygon_ID)),]$keep
controls_main <- subset(controls_main, keep==1)[,-ncol(controls_main)]
controls_main$treat <- 0
rm(d)
d <- rbind(projects_main, controls_main)
length(unique(d$polygon_ID))


d$def_p <- d$def/d$polygon_ha * 100 #proportional deforestation
d$buf1k_def_p <- d$buf1k_def/d$polygon_ha * 100
d$buf10k_def_p <- d$buf10k_def/d$polygon_ha * 100

data_colombia <- d
write.csv(unique(data_colombia$polygon_ID[ data_colombia$REDD==0 ]), "Matchit_controls_Colombia.csv")

set.seed(131313)
out1 <- gsynth(def_p ~ treat + mm + buf1k_def + buf10k_def,
               data = d,
               index = c("polygon_ID","year"),
               force = "two-way",
               EM = TRUE, #s = 500,
               min.T0 = 5,
               CV = TRUE, r = c(0, 5),
               #lambda = 10,
               estimator = "mc",
               se = TRUE, nboots = 1000)
out1
out1$est.att
write.csv(round(out1$est.att, digits = 4), "Colombia_GSC_ATT_Matchit.csv")
out1$est.avg
p1 <- plot(out1, xlab = "", xlim = c(-25,8), ylim = c(-2.1,1),
           ylab = "", main = "Colombia", theme.bw = F)
p2 <- plot(out1, type = "counterfactual", raw = "band", xlab = "Time relative to project implementation (years)",
           xlim = c(-25,8), ylim = c(-0.1,2),
           ylab = "", main = "", theme.bw = F, shade.post = FALSE,
           legendOff = T)

g1 <- plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(0.55, 0.5))


"###########################################################################################"
 #### Generalized Synthetic Control with MatchIt: Peru ####
"###########################################################################################"

setwd("West et al. (2023) data/Main datasets")
d <- read.csv("Peru_synth_control_data.csv")[,-1]
d[0,]
d$project <- NA
table(d$REDD)
unique(d$ID) #projects

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


redd.data <- d

d <- subset(d, year <= 2009)

d <- subset(d, treat==0)
d <- ddply(.data=d, .(project, polygon_ID), .fun=summarise,
           def = mean(def),
           REDD = mean(REDD),
           polygon_ha = mean(polygon_ha),
           pa = mean(pa),
           tree = mean(tree),
           dem = mean(dem),
           slope = mean(slope),
           fric = mean(fric),
           mm = mean(mm),
           buf1k_def = mean(buf1k_def),
           buf10k_def = mean(buf10k_def),
           IL = mean(IL),
           timber = mean(timber),
           mine = mean(mine))

colnames(d) <- c('Project','polygon_ID','Deforestation','REDD','Size',
                 'Protected_cover','Tree_cover','Elevation','Slope',
                 'Travel_distance','Precipitation','Buffer_1km','Buffer_10km',
                 "Indigenous_land", "Logging")

#run matching-------------------------------------------------------------------------------
m.out <- matchit(REDD ~ Deforestation + Protected_cover + Tree_cover + Elevation + Slope +
                   Travel_distance + Precipitation + Buffer_1km + Buffer_10km, # + Indigenous_land + Palm_oil,
                 data = d, method = "cardinal", ratio = 10, tols = 0.05, time = 600)

summary_peru <- summary(m.out, un = FALSE)

m.out2 <- m.out
cb2 <- love.plot(m.out2, abs = TRUE, binary = "std", thresholds = c(m = .1),
                 sample.names = c("Unmatched", "Matched"),
                 limits = c(0, 3),
                 position = "top",
                 shapes = c("circle", "triangle"),
                 colors = c("red", "blue")) + ggtitle("A. Covariate balance: Peruvian REDD+ sites")
cb2

#export matched data
m.data <- match.data(m.out, data = d)
m.data$keep <- 1

d <- redd.data #bring in the full data

projects_main <- subset(d, REDD==1)
unique(projects_main$polygon_ID)
controls_main <- subset(d, REDD==0)
controls_main$keep <- m.data[match(with(controls_main, polygon_ID), with(m.data, polygon_ID)),]$keep
controls_main <- subset(controls_main, keep==1)[,-ncol(controls_main)]
controls_main$treat <- 0
rm(d)
d <- rbind(projects_main, controls_main)
length(unique(d$polygon_ID))

d$def_p <- d$def/d$polygon_ha * 100 #proportional deforestation
d$buf1k_def_p <- d$buf1k_def/d$polygon_ha * 100
d$buf10k_def_p <- d$buf10k_def/d$polygon_ha * 100

data_peru <- d
write.csv(unique(data_peru$polygon_ID[ data_peru$REDD==0 ]), "Matchit_controls_Peru.csv")

set.seed(131313)
out <- gsynth(def_p ~ treat + mm + buf1k_def + buf10k_def,
              data = d,
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
write.csv(round(out$est.att, digits = 4), "Peru_GSC_ATT_Matchit.csv")
out$est.avg
p3 <- plot(out, xlab = "", xlim = c(-25,11), ylim = c(-2.1,1),
           ylab = "ATT on deforestation (%)", main = "Peru", theme.bw = F)
p4 <- plot(out, type = "counterfactual", raw = "band", xlab = " ",
           xlim = c(-25,11), ylim = c(-0.1,2),
           ylab = "Deforestation (%)", main = "", theme.bw = F, shade.post = FALSE,
           legendOff = T)

g2 <- plot_grid(p3, p4, align = "v", nrow = 2, rel_heights = c(0.55, 0.5))



"###########################################################################################"
 #### Generalized Synthetic Control with MatchIt: Africa ####
"###########################################################################################"

setwd("West et al. (2023) data/Main datasets")
congo <- read.csv("DRC_synth_control_data.csv")[,-1]
tanz <- read.csv("Tanzania_synth_control_data.csv")[,-1]
zamb <- read.csv("Zambia_synth_control_data.csv")[,-1]
congo[0,]
congo <- congo[,c(1:8,11,15,17,18,19,21,22,24)]
colnames(congo) <- c('ID','KML_ha','REDD','fid', 'polygon_ID','polygon_ha',
                     'year','def','pa','tree','dem','slope','fric','mm',
                     'buf1k_def','buf10k_def')
congo$startdate <- ifelse(congo$polygon_ID==0, 2008, 
                          ifelse(congo$polygon_ID==1, 2010, 0))
congo$treat <- 0
congo$treat[ congo$polygon_ID==0 & congo$year >= 2009 ] <- 1 
congo$treat[ congo$polygon_ID==1 & congo$year >= 2011 ] <- 1 

tanz[0,]
tanz <- tanz[,c(1:8,10,12,14,15,16,18,19,21)]
colnames(tanz) <- c('ID','KML_ha','REDD','fid', 'polygon_ID','polygon_ha',
                    'year','def','pa','tree','dem','slope','fric','mm',
                    'buf1k_def','buf10k_def')
tanz$startdate <- ifelse(tanz$polygon_ID==2933, 2011,
                         ifelse(tanz$polygon_ID==2934, 2017, 
                                ifelse(tanz$polygon_ID==2935, 2016, 0)))
tanz$treat <- 0
tanz$treat[ tanz$polygon_ID==2933 & tanz$year >= 2011 ] <- 1 
tanz$treat[ tanz$polygon_ID==2934 & tanz$year >= 2017 ] <- 1 
tanz$treat[ tanz$polygon_ID==2935 & tanz$year >= 2016 ] <- 1 
#differenciate the IDs:
tanz$polygon_ID <- tanz$polygon_ID + 1979

zamb[0,]
zamb <- zamb[,c(1:8,10,11,13,14,15,17,18,20)]
colnames(zamb) <- c('ID','KML_ha','REDD','fid', 'polygon_ID','polygon_ha',
                    'year','def','pa','tree','dem','slope','fric','mm',
                    'buf1k_def','buf10k_def')
zamb <- subset(zamb, ID != 15321 &  ID != 15322 & ID != 15323)
zamb$startdate <- ifelse(zamb$polygon_ID==6313, 2009,
                         ifelse(zamb$polygon_ID==6317, 2015,
                                ifelse(zamb$polygon_ID==6318, 2015, 
                                       ifelse(zamb$polygon_ID==6319, 2015, 0))))
zamb$treat <- 0
zamb$treat[ zamb$polygon_ID==6313 & zamb$year >= 2009 ] <- 1 
zamb$treat[ zamb$polygon_ID==6317 & zamb$year >= 2015 ] <- 1 
zamb$treat[ zamb$polygon_ID==6318 & zamb$year >= 2015 ] <- 1 
zamb$treat[ zamb$polygon_ID==6319 & zamb$year >= 2015 ] <- 1
#differenciate the IDs:
zamb$polygon_ID <- zamb$polygon_ID + 4915

#matching controls used in the SCs with their deforestation information:
redd.data <- rbind(congo, tanz, zamb)
redd.data$project <- redd.data$ID
redd.data$ID <- redd.data$polygon_ID #ID refers to polygon_ID!

d <- redd.data
d <- subset(d, treat==0)
d <- ddply(.data=d, .(project, polygon_ID), .fun=summarise,
           def = mean(def),
           REDD = mean(REDD),
           polygon_ha = mean(polygon_ha),
           pa = mean(pa),
           tree = mean(tree),
           dem = mean(dem),
           slope = mean(slope),
           fric = mean(fric),
           mm = mean(mm),
           buf1k_def = mean(buf1k_def),
           buf10k_def = mean(buf10k_def))

colnames(d) <- c('Project','polygon_ID','Deforestation','REDD','Size',
                 'Protected_cover','Tree_cover','Elevation','Slope',
                 'Travel_distance','Precipitation','Buffer_1km','Buffer_10km')

#run matching-------------------------------------------------------------------------------
m.out <- matchit(REDD ~ Deforestation + Protected_cover + Tree_cover + Elevation + Slope +
                   Travel_distance + Precipitation + Buffer_1km + Buffer_10km, # + Indigenous_land + Palm_oil,
                 data = d, method = "cardinal",  ratio = 10, tols = 0.025)

summary_africa <- summary(m.out, un = FALSE)

m.out3 <- m.out
cb3 <- love.plot(m.out3, abs = TRUE, binary = "std", thresholds = c(m = .1),
          sample.names = c("Unmatched", "Matched"),
          position = "top",
          limits = c(0, 3),
          shapes = c("circle", "triangle"),
          colors = c("red", "blue")) + ggtitle("C. Covariate balance: African REDD+ sites")

cb <- grid.arrange(cb2, cb1, cb3)
setwd("...")
ggsave(cb, units="cm", width=25, height=30, file="MatchIt_Cov_Balance.svg")
ggsave(cb, units="cm", dpi=600, width=25, height=30, file="MatchIt_Cov_Balance.png")


#export matched data
m.data <- match.data(m.out, data = d)
m.data$keep <- 1

d <- redd.data #bring in the full data

projects_main <- subset(d, REDD==1) #need to removed un-matched project
unique(projects_main$polygon_ID)
controls_main <- subset(d, REDD==0)
controls_main$keep <- m.data[match(with(controls_main, polygon_ID), with(m.data, polygon_ID)),]$keep
controls_main <- subset(controls_main, keep==1)[,-ncol(controls_main)]
controls_main$treat <- 0
rm(d)
d <- rbind(projects_main, controls_main)
length(unique(d$polygon_ID))

d$def_p <- d$def/d$polygon_ha * 100 #proportional deforestation
d$buf1k_def_p <- d$buf1k_def/d$polygon_ha * 100
d$buf10k_def_p <- d$buf10k_def/d$polygon_ha * 100

data_africa <- d
write.csv(unique(data_africa$polygon_ID[ data_africa$REDD==0 ]), "Matchit_controls_Africa.csv")

set.seed(131313)
out <- gsynth(def_p ~ treat + mm + buf1k_def + buf10k_def,
              data = d,
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
write.csv(round(out$est.att, digits = 4), "Africa_GSC_ATT_risk-based.csv")
out$est.avg
p5 <- plot(out, xlab = "", xlim = c(-25,11), ylim = c(-2.1,1),
           ylab = "", main = "Africa", theme.bw = F)
p6 <- plot(out, type = "counterfactual", raw = "band", xlab = " ",
           xlim = c(-30,30), ylim = c(-0.1,2),
           ylab = "", main = "", theme.bw = F, shade.post = FALSE,
           legendOff = T)

g3 <- plot_grid(p5, p6, align = "v", nrow = 2, rel_heights = c(0.55, 0.5))
