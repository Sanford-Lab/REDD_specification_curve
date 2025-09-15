# Author: Henry Chen, then Megan Ayers
# Date: 9/10/2025
# Function: Cleans CSVs to create dataframe for running synthetic controls.
# Saves the resulting data frame as a .Rdata file.

library(here)
library(tidyverse)
library(zoo)
library(tidyquant)

process <- function(project_name, overwrite = FALSE) {
  
  if (file.exists(paste0("data/processed/", project_name,
                         "/dat_synth.Rdata"))) {
    if (overwrite) {
      print(paste0("Processed file already exists for ", project_name,
                   ", but overwriting this as instructed."))
    } else {
      print(paste0("Processed file already exists for ", project_name,
                   ". Proceed with existing file, or set overwrite = TRUE."))
      return()
    }
  }
  
  # Define the base file path.
  base_path <- paste0("data/raw/", project_name, "/", project_name, "_exp/")
  
  # Initial forest cover in 2000.
  forest_2000 <- read_csv(paste0(base_path, "/forest_2000_", project_name,
                                 "_buffers.csv"), show_col_types = FALSE) %>% 
    select(-c(".geo")) %>%
    rename(treecover_2000 = "sum") %>%
    mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2",
                            1, 0))
  
  dat <- forest_2000
  
  
  # Now, collect forest loss for the rest of the years (1 through 22).
  for (i in 1:22) {
    loss <- read_csv(paste0(base_path, "/loss_", sprintf("%02d", i+2000), "_",
                            project_name, "_buffers.csv"),
                     show_col_types = FALSE) %>%
      select(-c(".geo")) %>%
      rename(!!paste0("loss_", sprintf("%02d", i)) := "sum") %>%
      mutate(treated = ifelse(str_sub(`system:index`,
                                      start = 1, end = 1) == "2", 1, 0))
    dat <- dat %>% left_join(loss, by = c("ID", "treated", "system:index"))
  }
  
  # Clean locations data set.
  locations <- read_csv(paste0(base_path, "/buffers_with_mean_citydist.csv"),
                        show_col_types = FALSE) %>% 
    select(-c(".geo")) %>%
    mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2",
                            1, 0))
  
  # ... and append it to dataframe.
  dat <- dat %>% left_join(locations, by = c("ID", "treated", "system:index"))
  
  
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
  
  
  # Save the Rdata file.
  # there may be other .Rdata files in the /processed/ directory, these are
  # outdated. From now on (Dec 2023), the final dataframe after processing will
  # be saved as dat_<method>.Rdata.
  save(dat_long, file = paste0("data/processed/", project_name,
                               "/dat_synth.Rdata"))
  
  # Also write it as a CSV (in case this is needed in the future).
  write.csv(dat_long, paste0("data/processed/", project_name, "/dat_synth.csv"))
}

