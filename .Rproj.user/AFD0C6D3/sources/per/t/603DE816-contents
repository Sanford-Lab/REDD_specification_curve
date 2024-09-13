"-----------------------------------------------------------------------------------"
"Note: Example of the standard synthetic control analyses for Colombia - Project 856"
"      Modifications to the code might be required for different projects/countries "
"-----------------------------------------------------------------------------------"

rm(list=ls())

library("Synth") #synthetic control package
library("MSCMT") #for a more robust optimization procedure
library("plyr")

#PART 1: Data preparation-----------------------------------------------------------------------------------------------
setwd("...")
redd.data <- read.csv("Colombia_synth_control_data.csv")[,-1]
redd.data$polygon_name <- as.character(redd.data$polygon_ID)

#remove problematic control (varies by project/country)
redd.data <- subset(redd.data, polygon_ID != 8851)

projects <- subset(redd.data, REDD == 1)
unique(projects$ID) 
#select one REDD project and delete the others and controls that should not be used
project.ID <- 856
redd.data <- subset(redd.data, ID == project.ID) #keep only controls specific for the project
project <- subset(redd.data, REDD == 1)

project.start.date <- 2011

redd.data[0,]

#fix polygon area based on an updated shapefile
setwd("...")
require(foreign)
new <- read.dbf("Colombia_final.dbf")
new$polygon_ha <- as.numeric(as.character(new$polygon_ha))
unique(new$polygon_ha)
redd.data$polygon_ha <- new[match(with(redd.data, polygon_ID), with(new, polygon_ID)),]$polygon_ha
unique(redd.data$polygon_ha)
setwd("...")

#set treatment.identifier
treatment.identifier <- subset(redd.data, REDD == 1)[1,5] #this is the polygon_ID
treatment.identifier #polygon_ID of the REDD project
redd.data[0,]

#calculate deforestation risk before project implementation
risk_project <- ddply(.data=subset(project, year <= project.start.date), .(polygon_ID), .fun=summarise, risk = mean(buf10k_def))
controls <- subset(redd.data, REDD != 1 & polygon_ID != treatment.identifier)
risk_control <- ddply(.data=subset(controls, year <= project.start.date), .(polygon_ID), .fun=summarise, risk = mean(buf10k_def))

#pre-subset
cutoff <- 0.2
risk_control$keep <- ifelse(risk_control$risk >= risk_project[1,2]*(1-cutoff) &
                              risk_control$risk <= risk_project[1,2]*(1+cutoff), 1, 0)
table(risk_control$keep)         

controls$keep <- risk_control[match(with(controls, polygon_ID), with(risk_control, polygon_ID)),]$keep
controls <- subset(controls, keep==1)
project$keep <- 1
redd.data <- rbind(project, controls)

#set controls.identifier
controls.identifier <- unique(controls$polygon_ID)  
controls.identifier

unique(redd.data$polygon_ha)
redd.data$polygon_ha <- round(redd.data$polygon_ha, digits = 0)

predictors.list <- c("tree", "IL", "pa", "slope", "dem", "fric", "def", "defc", "mm", "buf10k_def", "buf10k_defc")


#K fold---------------------------------------------------------------------------------------------------------
setwd("...")

project_name <- "Colombia 856"

for (i in 1:100) {
  tryCatch({   
    
    set.seed(131313+i)
    fold <- as.data.frame(sample(controls.identifier, round(length(controls.identifier)*0.8, digits = 0)))
    colnames(fold) <- "polygon_ID"
    fold$keep <- 1
    d <- redd.data 
    d$keep <- fold[match(with(d, polygon_ID), with(fold, polygon_ID)),]$keep
    d <- subset(d, keep==1 | REDD==1)
    d_controls <- subset(d, REDD==0)
    controls.identifier.loop <- unique(d_controls$polygon_ID) 
    
    #data preparation
    dataprep.out <-
      dataprep(foo = redd.data, #data name
               predictors =  predictors.list,
               predictors.op = "mean" , #variables based on means
               time.predictors.prior = 2001:c(project.start.date) , #interval used for pretreatment matching, varies by project
               dependent = "defc", #deforestation from Global Forest Cover dataset
               unit.variable = "polygon_ID",
               unit.names.variable = "polygon_name",
               time.variable = "year",
               treatment.identifier = treatment.identifier, #polygon that is REDD+ project under evaluation
               controls.identifier = controls.identifier.loop, #polygons that are not REDD+ projects
               time.optimize.ssr = 2001:c(project.start.date), #interval used for matching
               time.plot = 2001:2020 #time interval of entire analysis
      )
    
    #PART 2: Synthetic control analysis-------------------------------------------------------------------------------------
    #
    synth.out <- synth(data.prep.obj = dataprep.out)
    #checking results based on https://cran.r-project.org/web/packages/MSCMT/vignettes/CheckingSynth.html
    synth2.out <- improveSynth(synth.out, dataprep.out) 
    synth.out <- synth2.out
    
    #results
    gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
    gaps[, 1] #discrepancies in deforestation between REDD+ project and its synthetic control
    
    #result table
    synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
    #OBS: synth.tables has many comparison stats
    names(synth.tables)
    synth.tables$tab.pred #pre-treatment stats (Sample Mean = mean of control candidates)
    synth.tables$run <- i
    #write.csv(synth.tables$tab.pred, file = paste("904_matching_results_", selected_list[L], ".csv", sep = ""))
    
    print(subset(synth.tables$tab.w, w.weights != 0)) #% contribution from the control candidates used to construct the synthetic control
    #write.csv(subset(synth.tables$tab.w, w.weights != 0), file = paste("904_controls_used_", i, ".csv"))
    
    #plot pre- & post-deforestation trends
    path.plot(synth.res = synth.out,
              dataprep.res = dataprep.out,
              Ylab = "Deforestation (ha)", Xlab = "Year",
              #Ylim = c(-10,650),
              Legend.position = NA)
    abline(v=project.start.date, lty="dotted",lwd=2)
    
    #save results for ggplot
    ggplot.data <- as.data.frame(dataprep.out$Y1plot)
    ggplot.data[,2] <- dataprep.out$Y0plot %*% synth.out$solution.w
    ggplot.data[,3] <- "856"
    ggplot.data[,4] <- project.start.date
    colnames(ggplot.data) <- c("project.def", "synth.def", "project", "start")
    
    ggplot.data$loss.v <- as.vector(synth.out$loss.v)
    ggplot.data$loss.w <- as.vector(synth.out$loss.w)
    
    ggplot.data$run <- i
    ggplot.data$project <- project_name
    ggplot.data$type <- "cross_validation"
    write.csv(ggplot.data, paste("ggplot_856_main_", i, ".csv", sep = ""))
    
    project_gap <- gaps #save main gaps for the placebo plot
    print(i)
  }, error=function(e){})
}


#plot-----------------------------------------------------------------------------------------------------------
setwd("...")

synth = list.files(pattern="ggplot_856_*") #select raster names for the loop
a <- read.csv(synth[1])
a$run <- as.character(a$run)
for (i in 2:length(synth)) {
  temp <- read.csv(synth[i])
  temp$run <- as.character(temp$run)
  a <- rbind(a, temp)
  #rm(temp)
  print(synth[i])
}
colnames(a)[1] <- "year"
a$run <- as.factor(a$run)

require(plyr)
mspe <- subset(a, year < start)
mspe <- ddply(.data=mspe, .(run), .fun=summarise,
              mspe = mean((project.def - synth.def)^2))

#use official SC with all donors to calculate the MSPE
sc <- read.csv("...")
colnames(sc)[1] <- "year" 
sc_mspe <- subset(sc, year < project.start.date)
sc_mspe <- ddply(.data=sc_mspe, .(project), .fun=summarise, mspe = mean((project.def - synth.def)^2))[2]

mspe$keep <- ifelse(mspe$mspe < 1.5 * as.vector(sc_mspe$mspe), 1, 0) #drop large error runs
mspe <- subset(mspe, keep==1)

a$keep <- mspe[match(with(a, run), with(mspe, run)),]$keep
a <- subset(a, keep==1)

require(plyr)
interval <- ddply(.data=a, .(project, year), .fun=summarise,
                  min_inter = min(synth.def, na.rm=T),
                  max_inter = max(synth.def, na.rm=T)) #province level
write.csv(interval, "Project_856_cross_val.csv")


require(ggplot2)
g <- ggplot() +
  # geom_rect(aes(xmin = prior, xmax = startdate, ymin = 0, ymax = ymax),
  #           alpha = 0.25, fill = "#00BFC4",
  #           data = transform(dummy, ID = project),
  #           inherit.aes = FALSE) +
  #geom_point(data = dummy, aes(x=startdate+1, y=ymax), color="white", fill="white", size=0.1) +
  #add start date lines
  geom_vline(aes(xintercept = project.start.date), color="black", size = 0.5, linetype="dashed") +
  geom_line(aes(x=year, y=synth.def, group=run), colour="grey", a, size=1) +
  geom_ribbon(aes(x=year, ymin=min_inter, ymax=max_inter), interval, alpha=0.3, fill="#00BFC4") + #"cadetblue1"
  
  theme_bw() + 
  #theme(panel.grid.minor.y = element_blank()) +
  geom_line(aes(x=year, y=synth.def), color="#00BFC4", sc, linewidth=1) +
  geom_line(aes(x=year, y=project.def), color="red", a[c(1:20),], linewidth=1)  +
  
  theme(legend.position="") +
  #facet_wrap( . ~ project, ncol= 4, scales = "free") +
  labs(y = "Cumulative deforestation (ha)", x ="Year") +
  scale_x_continuous(breaks = c(2001,2007,2013,2019))
g
