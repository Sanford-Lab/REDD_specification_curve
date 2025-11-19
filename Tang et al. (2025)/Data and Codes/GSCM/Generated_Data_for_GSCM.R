library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(cowplot)

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
# You can change country_or_region_name to 'Brazil', 'Colombia', 'Africa', 'Southeast_Asia' and 'Pantropical'
country_or_region_name <- 'Peru'
target_country_or_region <- get(country_or_region_name)

# Import the table of project unit area
project_area <- read_excel("project_unit_area_ha.xlsx", sheet = "Sheet1")

# Define the function for completing the missing values (NA) in the data
fill_df_NA <- function(data) {
  df <- data %>%
    # Update the deforest_hotspot_dist value of 2001, using the value of 2002
    left_join(
      data %>% filter(year == 2002) %>% select(region, deforest_hotspot_dist_2002 = deforest_hotspot_dist),
      by = "region"
    ) %>%
    mutate(deforest_hotspot_dist = ifelse(year == 2001, deforest_hotspot_dist_2002, deforest_hotspot_dist)) %>%
    select(-deforest_hotspot_dist_2002) %>%
    
    # Update the water_dist and NPP values of 2022, using the values of 2021
    left_join(
      data %>% filter(year == 2021) %>% select(region, water_dist_2021 = water_dist, NPP_2021 = NPP),
      by = "region"
    ) %>%
    mutate(water_dist = ifelse(year == 2022, water_dist_2021, water_dist),
           NPP = ifelse(year == 2022, NPP_2021, NPP)) %>%
    select(-water_dist_2021,-NPP_2021)
  
  # Check for situation where year is 2020 and NPP value is NA, this is caused by the spatial gaps of MODIS NPP data set due to 
  # a variety of factors, such as sensor limitations, cloud cover, and data processing issues.
  # If such situation exist, use the average of the 'NPP' values from the years 2019 and 2021 to fill in the 'NPP' value for the year 2020.
  if (any(data$year == 2020 & is.na(data$NPP))) {
    df <- df %>%
      left_join(
        df %>% filter(year == 2019) %>% select(region, NPP_2019 = NPP),
        by = "region"
      ) %>%
      left_join(
        df %>% filter(year == 2021) %>% select(region, NPP_2021 = NPP),
        by = "region"
      ) %>%
      mutate(NPP = ifelse(year == 2020, (NPP_2019 + NPP_2021)/2, NPP)) %>%
      select(-NPP_2019, -NPP_2021)
  }
  # Check for situation where year is 2019 and NPP value is NA, update by the average of the 'NPP' values from the years 2018 and 2020
  if (any(data$year == 2019 & is.na(data$NPP))) {
    df <- df %>%
      left_join(
        df %>% filter(year == 2018) %>% select(region, NPP_2018 = NPP),
        by = "region"
      ) %>%
      left_join(
        df %>% filter(year == 2020) %>% select(region, NPP_2020 = NPP),
        by = "region"
      ) %>%
      mutate(NPP = ifelse(year == 2019, (NPP_2018 + NPP_2020)/2, NPP)) %>%
      select(-NPP_2018, -NPP_2020)
  }
  return(df)
}


# Process the GSCM_data_df to store data about project area and synthetic controls for GSCM modelling
GSCM_data_df <- data.frame()

for(project_id in target_country_or_region){
  # Obtain the project start year
  start_year <- all_project_startyear_list[which(Pantropical == project_id)]
  print(start_year)
  
  # Obtain project unit
  if (project_id %in% project_contain_subarea_id_list) {
    if (project_id != 1953){
      project_unit_list <- paste(project_id, 1:project_subarea_num_list[which(project_contain_subarea_id_list == project_id)], sep = "-")
    }else{
      project_unit_list <- paste(project_id, 3, sep = "-")
    }
  }else{
    project_unit_list <- as.character(project_id)
  }
  
  for(project_unit in project_unit_list){
    print(project_unit)
    
    # Obtain project unit area for calculating deforestation rate
    project_unit_area <-  as.numeric(project_area[project_area$project_unit == project_unit, "project_unit_ha"])
    
    # Import all instances
    for (i in 0:9) {
      # Original data path
      table_name <- paste0(project_unit,"_SCM_data_df_",i,".xlsx")
      table_path <- paste0("D:\\01Research\\01MyPapers\\09REDD VCS baseline\\Primary_results\\Table\\",
                           project_unit, "\\", table_name)
      table_data <- read_excel(table_path, sheet = "scm_modelling_dataset")
      
      # Complete the missing values
      df_temp <- fill_df_NA(table_data)
      
      # At this point, df_temp is in tibble format and needs to be converted to a traditional data frame
      df_temp <- as.data.frame(df_temp)
      
      # Add 'treated' column to indicate the time range when the project was implemented
      df_temp <- df_temp %>%
        mutate(treated = if_else(region == "project area" & year >= start_year, 1, 0))
      
      # Add 'deforest_rate' column and calculate deforestation rate
      names(df_temp)[names(df_temp) == 'deforest area'] <- 'deforest_area'
      df_temp$deforest_rate <- df_temp$deforest_area / project_unit_area * 100
      
      # Obtain synthetic control (SC) data from "weight" sheet derived from PSCM
      weight_df <- read_excel(table_path, sheet = "weight")
      first_column_data <- weight_df[[1]]
      matches <- grepl("donor", first_column_data, ignore.case = TRUE)
      SC_name <- first_column_data[matches]
      SC_name <- unlist(SC_name)
      print(SC_name)
      
      # Concatenate the data of project area and SC from multiple instances
      if(i == 0){
        df_temp <-  df_temp[df_temp$region %in% c("project area", SC_name), ]
      }else{
        df_temp <-  df_temp[df_temp$region %in% c(SC_name), ]
      }
      
      # Add project unit and instance numbers.
      df_temp <- df_temp %>%
        mutate(region = if_else(region == "project area", paste(project_unit, region), paste(project_unit, i, region)))
      
      # Add 'source_file' column to represent its original file name
      df_temp <- df_temp %>%
        mutate(source_file = table_name)
      
      # Append df_temp to GSCM_data_df
      if (nrow(GSCM_data_df) == 0) {
        GSCM_data_df <- df_temp
      } else {
        GSCM_data_df <- rbind(GSCM_data_df, df_temp)
      }
    }
  }
}

# Output GSCM_data_df to csv
output_filename <- paste0(country_or_region_name, "_sc_data_for_GSCM.csv")
write.csv(GSCM_data_df, file = output_filename, row.names = FALSE)