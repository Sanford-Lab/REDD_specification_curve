library(gsynth)
library(ggplot2)

setwd("...")
# REDD+ project ids of in each countries and regions
Brazil <- c(875, 963, 977, 981, 1112, 1113, 1115, 1118, 1329, 1503, 1571,
            1654, 1686, 1953, 2252, 2373, 2508, 2539, 2558, 2566)
Colombia <- c(856, 1389, 1390, 1391, 1392, 1395, 1396, 1399, 1400, 1566, 2723)
Peru <- c(844, 944, 958, 985, 1067, 1218, 1360, 1799, 1882, 2278, 2502)
Africa <- c(2510, 934, 1311, 1674, 1201, 1897, 1202, 1775)
Southeast_Asia <- c(904, 1650, 1398)
Pantropical <- c(875, 963, 977, 981, 1112, 1113, 1115, 1118, 1329, 1503, 1571,
                 1654, 1686, 1953, 2252, 2373, 2508, 2539, 2558, 2566, 856,
                 1389, 1390, 1391, 1392, 1395, 1396, 1399, 1400, 1566, 2723,
                 844, 944, 958, 985, 1067, 1218, 1360, 1799, 1882, 2278, 2502,
                 2510, 934, 1311, 1674, 1201, 1897, 1202, 1775, 904, 1650, 1398)
all_project_startyear_list <- c(2009, 2011, 2009, 2009, 2011, 2011, 2011, 2009, 2012,
                                2012, 2013, 2013, 2014, 2016, 2016, 2020, 2018, 2020,
                                2017, 2016, 2010, 2013, 2014, 2013, 2013, 2013, 2014,
                                2013, 2013, 2013, 2019, 2009, 2008, 2010, 2008, 2010,
                                2011, 2010, 2017, 2013, 2018, 2017, 2016, 2011, 2007,
                                2012, 2012, 2017, 2009, 2015, 2008, 2010, 2014)
# REDD+ projects with project unit divisions and the number of units
project_contain_subarea_id_list <- c(977,981,1953,2373,2508,2566,1389,1360,1775,2510)
project_subarea_num_list <- c(3,3,3,2,2,3,2,3,3,1)

# Obtain the data of target country, take Peru as an example here.
# You can change it to 'Brazil', 'Colombia', 'Africa', 'Southeast_Asia' and 'Pantropical'
country_or_region_name <- 'Peru'
target_country_or_region <- get(country_or_region_name)

# Import the data about project area and synthetic controls for GSCM modelling
table_name <- paste0(country_or_region_name, "_sc_data_for_GSCM.csv")
sc_data <- read.csv(table_name)

# Perform GSCM modeling
set.seed(1013)
GSCM_out <- gsynth(deforest_rate ~ treated + road_dist + water_dist +
                   deforest_hotspot_dist + settlement_cluster_dist + NPP,
                   data = sc_data,
                   index = c("region","year"),
                   force = "two-way",
                   EM = TRUE, 
                   min.T0 = 7,
                   CV = TRUE, r = c(0, 5),
                   estimator = "mc",
                   se = TRUE, 
                   nboots = 1000)

p1 <- plot(GSCM_out, 
           xlab = "",
           ylab = "ATT on deforestation rate (%)", 
           main = country_or_region_name, 
           theme.bw = F)
p2 <- plot(GSCM_out, type = "counterfactual", 
           raw = "band", xlab = " ",
           ylab = "Deforestation rate (%)", 
           main = "", 
           theme.bw = F, 
           shade.post = FALSE,
           legendOff = T)

g1 <- plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(0.55, 0.5))
g1

ggsave(g1, units="cm", width=16, height=16, file=paste0(country_or_region_name, "_GSCM_results.emf"))
ggsave(g1, dpi=600, units="cm", width=16, height=16, file=paste0(country_or_region_name, "_GSC_results.png"))

# Save the result to txt
sink(paste0(country_or_region_name, "_GSCM_results.txt"), append = FALSE)
print(GSCM_out)
sink()
print(GSCM_out)