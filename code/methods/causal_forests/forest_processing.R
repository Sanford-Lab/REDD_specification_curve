# Author: Nick Wu
# Date: 3/28/2024
# Function: Cleans CSVs to create dataframe that is conducive of running the 
# causal forests procedure on. saves this data frame as a .Rdata file.

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




