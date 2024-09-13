'################################################################################'
'##### CODE FOR CROSS-SECTION ANALYSIS OF REDD+ PROJECTS (Imai et al. 2021) #####'
'################################################################################'

require(PanelMatch)
require(ggplot2)
require(ggplot2)
require(plyr)
require(gridExtra)

#data preparation------------------------------------------------------------------------------
setwd("West et al. (2023) data/Main datasets")
d1 <- read.csv("Cambodia_synth_control_data.csv")[,-1]
d1 <- subset(d1, REDD==1)
setwd("...")
d2 <- read.csv("Colombia_synth_control_data.csv")[,-1]
d2 <- subset(d2, REDD==1)
setwd("...")
d3 <- read.csv("Peru_synth_control_data.csv")[,-1]
d3 <- subset(d3, REDD==1)
setwd("...")
d4 <- read.csv("DRC_synth_control_data.csv")[,-1]
d4 <- subset(d4, REDD==1)
setwd("...")
d5 <- read.csv("Tanzania_synth_control_data.csv")[,-1]
d5 <- subset(d5, REDD==1)
setwd("...")
d6 <- read.csv("Zambia_synth_control_data.csv")[,-1]
d6 <- subset(d6, REDD==1)

d1[0,]
d2[0,]
d3[0,]
d4[0,]
d5[0,]
d6[0,]

d1 <- d1[,c('ID','polygon_ha','year','def','pa','tree','mm','dem','slope','fric','buf1k_def','buf10k_def')]
d2 <- d2[,c('ID','polygon_ha','year','def','pa','tree','mm','dem','slope','fric','buf1k_def','buf10k_def')]
d3 <- d3[,c('ID','polygon_ha','year','def','pa','tree','mm','dem','slope','fric','buf1k_def','buf10k_def')]
d4 <- d4[,c('ID','polygon_ha','year','def','pa','tree','mm','dem','slope','fric','buf1k_def','buf10k_def')]
d5 <- d5[,c('ID','polygon_ha','year','def','pa','tree','mm','dem','slope','fric','buf1k_def','buf10k_def')]
d6 <- d6[,c('ID','polygon_ha','year','def','pa','tree','mm','dem','slope','fric','buf1k_def','buf10k_def')]

d <- rbind(d1,d2,d3,d4,d5,d6)
rm(d1,d2,d3,d4,d5,d6)

#remove problematic projects
d <- subset(d, ID != 1399 & ID != 868 & ID != 15321 & ID != 15322 & ID != 15323)

unique(d$ID)
d$ID[ d$ID==1182] <- 1882

d$start[ d$ID==1650] <- 2010
d$start[ d$ID==904] <- 2008
d$start[ d$ID==1748] <- 2015
d$start[ d$ID==1392] <- 2013
d$start[ d$ID==1566] <- 2013
d$start[ d$ID==856] <- 2011
d$start[ d$ID==1391] <- 2013
d$start[ d$ID==1396] <- 2014
d$start[ d$ID==1400] <- 2013
d$start[ d$ID==1389] <- 2013
d$start[ d$ID==1395] <- 2013
d$start[ d$ID==13603] <- 2010
d$start[ d$ID==844] <- 2009
d$start[ d$ID==944] <- 2009
d$start[ d$ID==985] <- 2009
d$start[ d$ID==958] <- 2011
d$start[ d$ID==13602] <- 2010
d$start[ d$ID==2278] <- 2018
d$start[ d$ID==13601] <- 2010
d$start[ d$ID==1882] <- 2013
d$start[ d$ID==1359] <- 2009
d$start[ d$ID==934] <- 2011
d$start[ d$ID==1325] <- 2011
d$start[ d$ID==1897] <- 2017
d$start[ d$ID==1900] <- 2016
d$start[ d$ID==17751] <- 2015
d$start[ d$ID==17752] <- 2015
d$start[ d$ID==17753] <- 2015
d$start[ d$ID==1202] <- 2009
d$start[ d$ID==1390] <- 2014
d$start[ d$ID==1067] <- 2011

table(is.na(d$start)) #all start dates complete

#treatment dummy------------------------------------------------------------------------------
d$treat_dummy <- ifelse(d$year >= d$start, 1, 0)

#analysis-------------------------------------------------------------------------------------
setwd("C:/Users/tpt590/OneDrive - Vrije Universiteit Amsterdam/REDD analysis Risk")

p <- DisplayTreatment(unit.id = "ID",
                      title = "Voluntary REDD+ project implementation",
                      legend.labels = c('Pre-project period','Post-project project'),
                      time.id = "year", legend.position = "bottom",
                      xlab = "Year", ylab = "Project ID",
                      treatment = "treat_dummy", data = d)

ggsave(p, dpi=600, units="cm", width=20, height=15, file="project_implementation_display.svg")


#Absolute deforestation analysis--------------------------------------------------------------
lags_n <- 3 #c(1:3)
size_match_n <- 1 #c(1,5,10)
lead_upper_range <- 0:4

res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope',#time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "none", #no matching
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+ #slope+#time-invarriant
                                               buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_none_projects_abs.csv")
write.csv(res_cov, "cov_balance_none_abs.csv")


res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope', #time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "mahalanobis",
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+#slope+ #time-invarriant
                                 buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_MD_projects_abs.csv")
write.csv(res_cov, "cov_balance_MD_abs.csv") 


res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope', #time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "ps.match", 
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+#slope+ #time-invarriant
                                 buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree', 'mm', 'dem','fric', #'pa',  #'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_PSM_projects_abs.csv")
write.csv(res_cov, "cov_balance_PSM_abs.csv")


res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope' #time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "ps.weight",
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+# slope+  #time-invarriant
                                 buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  # 'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_PSW_projects_abs.csv")
write.csv(res_cov, "cov_balance_PSW_abs.csv")


#Relative deforestation analysis--------------------------------------------------------------
d$def <- d$def/d$polygon_ha * 100
d$buf1k_def <- d$buf1k_def/d$polygon_ha * 100
d$buf10k_def <- d$buf10k_def/d$polygon_ha * 100


res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope' #time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "none", #no matching
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+# slope+  #time-invarriant
                                 buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  # 'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_none_projects_perc.csv")
write.csv(res_cov, "cov_balance_none_perc.csv")


res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope' #time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "mahalanobis",
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+# slope+  #time-invarriant
                                 buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  # 'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_MD_projects_perc.csv")
write.csv(res_cov, "cov_balance_MD_perc.csv") 


res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope' #time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "ps.match", 
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+# slope+  #time-invarriant
                                 buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  # 'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_PSM_projects_perc.csv")
write.csv(res_cov, "cov_balance_PSM_perc.csv")


res <- as.data.frame(matrix(NA,0,7)) #table to save results from loop
colnames(res) <- c('estimate','std.error','2.5%','97.5%','lag','match_size','lead')
res_cov <- as.data.frame(matrix(NA,0,13)) #table to save results from loop
colnames(res_cov) <- c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  #'slope' #time-invarriant
                       'buf1k_def', 'buf10k_def', #time-variant
                       'lag','match_size','lead','time') #model settings
control_list <- as.data.frame(matrix(NA,0,1)) #save the controls used
colnames(control_list)[1] <- "weigths"
control_list$ID <- as.integer(as.vector(rownames(control_list)))
for (l in 1:length(lags_n)) {
  for (m in 1:length(size_match_n)) {
    for (u in 1:length(lead_upper_range)) {
      PM.results <- PanelMatch(lag = lags_n[l], #for loop
                               time.id = "year", unit.id = "ID", 
                               treatment = "treat_dummy", 
                               refinement.method = "ps.weight",
                               data = d, match.missing = TRUE, 
                               covs.formula = ~polygon_ha+ tree+ dem+ fric+ mm+ # pa+# slope+  #time-invarriant
                                 buf1k_def+ buf10k_def, #time-variant
                               size.match = size_match_n[m], #for loop
                               qoi = "att" ,
                               outcome.var = "def",
                               lead = 0:lead_upper_range[u], #for loop
                               forbid.treatment.reversal = TRUE, 
                               use.diagonal.variance.matrix = TRUE)
      #save covariates results
      cov <- get_covariate_balance(PM.results$att,
                                   data = d,
                                   covariates = c('polygon_ha', 'tree',  'dem', 'fric', 'mm', # 'pa',  # 'slope',
                                                  'buf1k_def', 'buf10k_def'),
                                   plot = F, legend = F)
      cov <- as.data.frame(cov)
      cov$lag <- lags_n[l]
      cov$match_size <- size_match_n[m]
      cov$lead <-lead_upper_range[u]
      cov$time <- row.names(cov)
      res_cov <- rbind(res_cov, cov) #merge results from loop with the res table
      PE.results <- PanelEstimate(sets = PM.results, data = d,
                                  number.iterations = 1000,
                                  df.adjustment = T,
                                  confidence.level = 0.95)
      # PE.results[["estimates"]]
      # summary(PE.results)
      # plot(PE.results)
      temp <- summary(PE.results)
      temp <- as.data.frame(temp$summary)
      temp$lag <- lags_n[l]
      temp$match_size <- size_match_n[m]
      temp$lead <-lead_upper_range[u]
      temp$time <- row.names(temp)
      res <- rbind(res, temp) #merge results from loop with the res table
    }
  }
}
write.csv(res, "ATT_PSW_projects_perc.csv")
write.csv(res_cov, "cov_balance_PSW_perc.csv")

p <- DisplayTreatment(unit.id = "ID", time.id = "year", treatment = 'treat_dummy', 
                 data = d, matched.set = PM.results$att[20], 
                 title = "Selected controls for Project 1650 in 2010 based on propensity score weighting",
                 legend.labels = c('Pre-project period','Post-project project'),
                 #legend.position = "bottom",
                 xlab = "Year", ylab = "Project ID",
                 show.set.only = T)

ggsave(p, dpi=600, units="cm", width=20, height=15, file="example_selected_controls.svg")


#plot results----------------------------------------------------------------------------------
setwd("...")
#p0 <- read.csv("ATT_none_projects_abs.csv")[,-1]
#p0 <- read.csv("ATT_none_projects_perc.csv")[,-1]
p1 <- read.csv("ATT_MD_projects_abs.csv")[,-1]
p1$method <- "Mahalanobis distance"
p1$type <- "Absolute deforestation"
p2 <- read.csv("ATT_MD_projects_perc.csv")[,-1]
p2$method <- "Mahalanobis distance"
p2$type <- "Relative deforestation"
p3 <- read.csv("ATT_PSM_projects_abs.csv")[,-1]
p3$method <- "Propensity score matching"
p3$type <- "Absolute deforestation"
p4 <- read.csv("ATT_PSM_projects_perc.csv")[,-1]
p4$method <- "Propensity score matching"
p4$type <- "Relative deforestation"
p5 <- read.csv("ATT_PSW_projects_abs.csv")[,-1]
p5$method <- "Propensity score weighting"
p5$type <- "Absolute deforestation"
p6 <- read.csv("ATT_PSW_projects_perc.csv")[,-1]
p6$method <- "Propensity score weighting"
p6$type <- "Relative deforestation"

d_plot <- rbind(p1,p2,p3,p4,p5,p6)

#annual ATTs
d_plot$annual_estimate <- ifelse(d_plot$time=="t+0", d_plot$estimate,
                                 ifelse(d_plot$time=="t+1", d_plot$estimate/2,
                                        ifelse(d_plot$time=="t+2", d_plot$estimate/3,
                                               ifelse(d_plot$time=="t+3", d_plot$estimate/4,
                                                      ifelse(d_plot$time=="t+4", d_plot$estimate/5, NA)))))

ddply(.data=d_plot, .(time), .fun=summarise, def = mean(annual_estimate))

#overlapping SE
d_plot$estimate_minus_SE <- d_plot$estimate + d_plot$std.error
sum(d_plot$estimate_minus_SE < 0)/nrow(d_plot)
ddply(.data=subset(d_plot, estimate_minus_SE < 0), .(time), .fun=summarise, def = mean(annual_estimate))

d_plot$year <- ifelse(d_plot$time=="t+0", 1,
                      ifelse(d_plot$time=="t+1", 2,
                             ifelse(d_plot$time=="t+2", 3,
                                    ifelse(d_plot$time=="t+3", 4,
                                           ifelse(d_plot$time=="t+4", 5,NA)))))

d_plot$lead <- d_plot$lead + 1

d_plot$lead[ d_plot$lead==1 ] <- "1 year"
d_plot$lead[ d_plot$lead==2 ] <- "2 years"
d_plot$lead[ d_plot$lead==3 ] <- "3 years"
d_plot$lead[ d_plot$lead==4 ] <- "4 years"
d_plot$lead[ d_plot$lead==5 ] <- "5 years"

g1 <- ggplot(subset(d_plot, type=='Absolute deforestation'), aes(x=year, y=estimate, colour=as.factor(lead), shape=as.factor(lead))) + 
  #geom_hline(yintercept=0, color = "grey75", linetype='solid') + 
  theme_bw() + geom_hline(yintercept=0, linetype='dashed') +
  geom_point(position=position_dodge(.6)) +
  geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0, position=position_dodge(.6)) + #CI figs
  #geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), alpha=0.3, size=2, width=0, position=position_dodge(.6)) + #CI figs
  #geom_errorbar(aes(ymin=c(estimate-std.error), ymax=c(estimate+std.error)), width=0, position=position_dodge(.6)) +
  facet_wrap(method ~ ., ncol = 1) +
  #facet_wrap(. ~ lag) +
  labs(colour = "Evaluation period", shape = "Evaluation period") +
  theme(legend.position = "none") +
  #theme( panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  #theme( panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  theme( panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank()) +
  #theme( panel.grid = element_blank() ) +
  labs(x = "Years after project start", y = "Average REDD+ impact on absolute deforestation (ha)")
g1

g2 <- ggplot(subset(d_plot, type=='Relative deforestation'), aes(x=year, y=estimate, colour=as.factor(lead), shape=as.factor(lead))) + 
  #geom_hline(yintercept=0, color = "grey75", linetype='solid') + 
  theme_bw() + geom_hline(yintercept=0, linetype='dashed') +
  geom_point(position=position_dodge(.6)) +
  geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0, position=position_dodge(.6)) + #CI figs
  #geom_errorbar(aes(ymin=c(estimate-std.error), ymax=c(estimate+std.error)), width=0, position=position_dodge(.6)) +
  facet_wrap(method ~ ., ncol = 1) +
  #facet_wrap(. ~ lag) +
  labs(colour = "Evaluation period", shape = "Evaluation period") +
  #theme(legend.position = "none") +
  #theme( panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  #theme( panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  theme( panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank()) +
  #theme( panel.grid = element_blank() ) +
  labs(x = "Years after project start", y = "Average REDD+ impact on relative deforestation (%)")
g2

g <- grid.arrange(g1, g2, ncol = 2, widths=c(0.69,1))
ggsave(g, units="cm", width=18, height=14, file="Imai_REDD_ATT.svg", dpi=600)
ggsave(g, units="cm", width=18, height=14, file="Imai_REDD_ATT.png", dpi=600)


