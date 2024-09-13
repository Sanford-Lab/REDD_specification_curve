
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
  
  print(summary(dat_long))
  dat_long <- drop_na(dat_long)
  
  save(dat_long, file = paste0("data/processed/", project_name, "/dat_matching.Rdata"))
}


process_synth_data <- function(project_name) {
  
  library(tidyverse)
  
  # Define the base file path
  base_path <- paste0("data/raw/", project_name, "/", project_name, "_exp/")
  
  # Read in the forest_2000 file
  forest_2000 <- read_csv(paste0(base_path, "forest_2000_", project_name, "_buffers.csv")) %>% 
    select(-c(".geo")) %>%
    rename(treecover_2000 = "sum") %>%
    mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
  
  # Initialize the data as forest_2000
  dat <- forest_2000
  
  # Loop through the years, reading in the loss files and joining to data
  for (i in 1:22) {
    year_suffix <- sprintf("%02d", i)  # Add leading zeros
    file_path <- paste0(base_path, "loss_", 2000 + i, "_", project_name, "_buffers.csv")
    
    # Read in the loss file
    loss <- read_csv(file_path) %>% 
      select(-c(".geo")) %>% 
      rename(!!paste0("loss_", year_suffix) := "sum") %>%
      mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
    
    # Join to data
    dat <- dat %>% left_join(loss)
  }
  
  # Read in and join the locations file
  locations <- read_csv(paste0(base_path, "buffers_with_mean_citydist.csv")) %>% 
    select(-c(".geo")) %>%
    mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
  
  dat <- dat %>% left_join(locations)
  
  # Transform data to long format and calculate cumulative loss and remaining tree cover
  dat_long <- dat %>% #mutate(ID = row_number()) %>% 
    pivot_longer(cols = starts_with("loss"),
                 names_to = "year",
                 names_prefix = "loss_",
                 values_to = "loss") %>%
    group_by(ID) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(cum_loss = cumsum(loss),
           treecover_remaining = treecover_2000 - cum_loss,
           year = as.numeric(year)) %>%
    rename(elevation = be75)
  
  print(summary(dat_long))
  dat_long <- drop_na(dat_long)
  
  # Save the processed data
  save(dat_long, file = paste0("data/processed/", project_name, "/dat_synth.Rdata"))
}

