# Author: Unknown
# Date: 12/23/2023
# Function: Cleans CSVs to create dataframe that is conducive of running the matching
# procedure on. saves this data frame as a .Rdata file.

process_matching_data <- function(project_name) {
  
  library(tidyverse)
  library(zoo)
  library(tidyquant)
  
  # Define the base file path
  base_path <- paste0("data/raw/", project_name, "/", project_name, "_exp/")
  
  forest_2000 <- read_csv(paste0(base_path, "/forest_2000_", project_name, "_points.csv")) %>% 
    select(-c(".geo")) %>%
    rename(treecover_2000 = "sum")
  
  dat <- forest_2000
  
  for (i in 1:22) {
    loss <- read_csv(paste0(base_path, "/loss_", sprintf("%02d", i+2000), "_", project_name, "_points.csv")) %>% 
      select(-c(".geo")) %>% 
      rename(!!paste0("loss_", sprintf("%02d", i)) := "sum")
    dat <- dat %>% left_join(loss)
  }
  
  locations <- read_csv(paste0(base_path, "/points_with_mean_citydist.csv")) %>% 
    select(-c(".geo"))
  
  dat <- dat %>% left_join(locations)
  
  # View(dat)
  
  deforestation_distance <- read_csv(paste0(base_path, "/points_with_defo_distance.csv")) %>% 
    select(-c(".geo")) %>% pivot_longer(cols = starts_with("distance"),
                                        names_to = "year",
                                        names_prefix = "distance_",
                                        values_to = "defo_distance") %>%
    mutate(year = as.numeric(year))
  
  dat_long <- dat %>% pivot_longer(cols = starts_with("loss_"),
                                   names_to = "year",
                                   names_prefix = "loss_",
                                   values_to = "loss") %>%
    group_by(ID) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(cum_loss = cumsum(loss),
           treecover_remaining = treecover_2000 - cum_loss,
           year = as.numeric(year)) %>%
    rename(elevation = be75) %>%
    left_join(deforestation_distance)
  
  # print(summary(dat_long))
  dat_long <- drop_na(dat_long)
  
  # there may be other .Rdata files in the /processed/ directory, these are outdated.
  # from now on (dec 2023), the final dataframe after processing will be saved as
  # dat_<method>.Rdata
  save(dat_long, file = paste0("data/processed/", project_name, "/dat_matching.Rdata"))
  
  # write as a CSV for future reference
  write.csv(dat_long, paste0("data/processed/", project_name, "/dat_matching.csv"))
}


process_synth_data <- function(project_name) {
  library(tidyverse)
  library(zoo)
  library(tidyquant)
  
  # Define the base file path
  base_path <- paste0("data/raw/", project_name, "/", project_name, "_exp/")
  
  # Set starting point
  forest_2000 <- read_csv(paste0(base_path, "/forest_2000_", project_name, "_buffers.csv")) %>% 
    select(-c(".geo")) %>%
    rename(treecover_2000 = "sum") %>%
    mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
  
  dat <- forest_2000
  
  
  # Now, do the rest of the years (1 through 22)
  for (i in 1:22) {
    loss <- read_csv(paste0(base_path, "/loss_", sprintf("%02d", i+2000), "_", project_name, "_buffers.csv")) %>%
      select(-c(".geo")) %>%
      rename(!!paste0("loss_", sprintf("%02d", i)) := "sum") %>%
      mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
    dat <- dat %>% left_join(loss)
  }
  
  # Clean locations data set too
  locations <- read_csv(paste0(base_path, "/buffers_with_mean_citydist.csv")) %>% 
    select(-c(".geo")) %>%
    mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
  
  # ... and append it to dataframe
  dat <- dat %>% left_join(locations)
  
  # View(dat)
  
  dat_long <- dat %>% mutate(ID = row_number()) %>% 
    pivot_longer(cols = loss_01:loss_22,
                 names_to = "year",
                 names_prefix = "loss_",
                 values_to = "loss") %>%
    group_by(ID) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(cum_loss = cumsum(loss),
           treecover_remaining = treecover_2000 - cum_loss,
           year = as.numeric(year)) %>%
    rename(elevation = be75)
  
  View(dat_long)
  
  # Save the Rdata file
  # there may be other .Rdata files in the /processed/ directory, these are outdated.
  # from now on (dec 2023), the final dataframe after processing will be saved as
  # dat_<method>.Rdata
  save(dat_long, file = paste0("data/processed/", project_name, "/dat_synth.Rdata"))
  
  # also write it as a CSV (in case this is needed in the future)
  write.csv(dat_long, paste0("data/processed/", project_name, "/dat_synth.csv"))
}



process_forest_data <- function(project_name) {
  
  library(tidyverse)
  library(zoo)
  library(tidyquant)
  
  # Define the base file path
  base_path <- paste0("data/raw/", project_name, "/", project_name, "_exp/")
  
  forest_2000 <- read_csv(paste0(base_path, "/forest_2000_", project_name, "_points.csv")) %>% 
    select(-c(".geo")) %>%
    rename(treecover_2000 = "sum")
  
  dat <- forest_2000
  
  for (i in 1:22) {
    loss <- read_csv(paste0(base_path, "/loss_", sprintf("%02d", i+2000), "_", project_name, "_points.csv")) %>% 
      select(-c(".geo")) %>% 
      rename(!!paste0("loss_", sprintf("%02d", i)) := "sum")
    dat <- dat %>% left_join(loss)
  }
  
  locations <- read_csv(paste0(base_path, "/points_with_mean_citydist.csv")) %>% 
    select(-c(".geo"))
  
  dat <- dat %>% left_join(locations)
  
  # View(dat)
  
  deforestation_distance <- read_csv(paste0(base_path, "/points_with_defo_distance.csv")) %>% 
    select(-c(".geo")) %>% pivot_longer(cols = starts_with("distance"),
                                        names_to = "year",
                                        names_prefix = "distance_",
                                        values_to = "defo_distance") %>%
    mutate(year = as.numeric(year))
  
  dat_long <- dat %>% pivot_longer(cols = starts_with("loss_"),
                                   names_to = "year",
                                   names_prefix = "loss_",
                                   values_to = "loss") %>%
    group_by(ID) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(cum_loss = cumsum(loss),
           treecover_remaining = treecover_2000 - cum_loss,
           year = as.numeric(year)) %>%
    rename(elevation = be75) %>%
    left_join(deforestation_distance)
  
  # print(summary(dat_long))
  dat_long <- drop_na(dat_long)
  
  # there may be other .Rdata files in the /processed/ directory, these are outdated.
  # from now on (dec 2023), the final dataframe after processing will be saved as
  # dat_<method>.Rdata
  save(dat_long, file = paste0("data/processed/", project_name, "/dat_forest.Rdata"))
  
  # write as a CSV for future reference
  write.csv(dat_long, paste0("data/processed/", project_name, "/dat_forest.csv"))
}



run_all_processing <- function(){
  raw_data_folder <- "data/raw"
  project_names <- list.dirs(raw_data_folder, recursive = FALSE, full.names = TRUE)
  
  # Check if folder exists under data/processed; if not, create it
  processed_data_folder <- "data/processed"
  if (!dir.exists(processed_data_folder)){
    dir.create(processed_data_folder)
  }
  
  for (full_project_path in project_names){
    project_name <- basename(full_project_path)  # Extract the folder name without the full path
    print(project_name)
    
    # Create project-specific folder under data/processed
    project_data_folder <- file.path(processed_data_folder, project_name)
    if (!dir.exists(project_data_folder)){
      dir.create(project_data_folder)
    }
    
    # Call the processing functions with the actual project name (folder name)
    process_matching_data(project_name)
    process_forest_data(project_name)
    process_synth_data(project_name)
  }
}
run_all_processing()
