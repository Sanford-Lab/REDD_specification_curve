# Author: Previously unknown, Megan Ayers as of 2025
# Date: 9/4/2025
# Function: Cleans CSVs to create dataframe that is conducive for running
# matching. Saves this data frame as a .Rdata file.

if (interactive()) {
  library(tidyverse)
  library(zoo)
  library(tidyquant)
  }

process <- function(project_name, overwrite = FALSE) {
  
  if (file.exists(paste0("data/processed/", project_name,
                         "/dat_matching.Rdata"))) {
    if (overwrite) {
      print(paste0("Processed file already exists for ", project_name,
                   ", but overwriting this as instructed."))
    } else {
      print(paste0("Processed file already exists for ", project_name,
                   ". Proceed with existing file, or set overwrite = TRUE."))
      return()
    }
  }
  
  # Define the base file path
  base_path <- paste0("data/raw/", project_name, "/", project_name, "_exp/")
  
  forest_2000 <- read_csv(paste0(base_path, "/forest_2000_", project_name,
                                 "_points.csv"), show_col_types = FALSE) %>% 
    select(-c(".geo")) %>%
    rename(treecover_2000 = "sum")
  
  dat <- forest_2000
  
  for (i in 1:22) {
    loss <- read_csv(paste0(base_path, "/loss_", sprintf("%02d", i+2000), "_",
                            project_name, "_points.csv"),
                     show_col_types = FALSE) %>% 
      select(-c(".geo")) %>%
      select(ID, sum) %>%
      rename(!!paste0("loss_", sprintf("%02d", i)) := "sum")
    dat <- dat %>% left_join(loss, by = "ID")
  }
  
  locations <- read_csv(paste0(base_path, "/points_with_mean_citydist.csv"),
                        show_col_types = FALSE) %>% 
    select(-c(".geo", "system:index", "treated"))
  
  dat <- dat %>% left_join(locations, by = "ID")
  
  
  deforestation_distance <- read_csv(paste0(base_path,
                                            "/points_with_defo_distance.csv"),
                                     show_col_types = FALSE) %>% 
    select(-c(".geo")) %>% pivot_longer(cols = starts_with("distance"),
                                        names_to = "year",
                                        names_prefix = "distance_",
                                        values_to = "defo_distance") %>%
    mutate(year = as.numeric(year)) %>%
    select(ID, year, defo_distance)
  
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
    left_join(deforestation_distance, by = c("ID", "year"))
  
  # print(summary(dat_long))
  dat_long <- drop_na(dat_long)
  
  # there may be other .Rdata files in the /processed/ directory, these are outdated.
  # from now on (dec 2023), the final dataframe after processing will be saved as
  # dat_<method>.Rdata
  save(dat_long, file = paste0("data/processed/", project_name, "/dat_matching.Rdata"))
  
  # write as a CSV for future reference
  write.csv(dat_long, paste0("data/processed/", project_name, "/dat_matching.csv"))
}



