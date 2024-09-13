# This gets synthetic control donor areas for ADMPL

source("code/functions.R")
process_synth_data("adpml")

library(tidyverse)
library(zoo)
 library(tidyquant)
 
 forest_2000 <- read_csv("data/raw/adpml/adpml_exp/forest_2000_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>%
   rename(treecover_2000 = "sum") %>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_01 <- read_csv("data/raw/adpml/adpml_exp/loss_2001_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_01 = "sum") %>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_02 <- read_csv("data/raw/adpml/adpml_exp/loss_2002_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_02 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_03 <- read_csv("data/raw/adpml/adpml_exp/loss_2003_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_03 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_04 <- read_csv("data/raw/adpml/adpml_exp/loss_2004_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_04 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_05 <- read_csv("data/raw/adpml/adpml_exp/loss_2005_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_05 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_06 <- read_csv("data/raw/adpml/adpml_exp/loss_2006_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_06 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_07 <- read_csv("data/raw/adpml/adpml_exp/loss_2007_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_07 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_08 <- read_csv("data/raw/adpml/adpml_exp/loss_2008_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_08 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_09 <- read_csv("data/raw/adpml/adpml_exp/loss_2009_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_09 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_10 <- read_csv("data/raw/adpml/adpml_exp/loss_2010_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_10 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_11 <- read_csv("data/raw/adpml/adpml_exp/loss_2011_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_11 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_12 <- read_csv("data/raw/adpml/adpml_exp/loss_2012_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_12 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_13 <- read_csv("data/raw/adpml/adpml_exp/loss_2013_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_13 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_14 <- read_csv("data/raw/adpml/adpml_exp/loss_2014_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_14 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_15 <- read_csv("data/raw/adpml/adpml_exp/loss_2015_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_15 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_16 <- read_csv("data/raw/adpml/adpml_exp/loss_2016_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_16 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_17 <- read_csv("data/raw/adpml/adpml_exp/loss_2017_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_17 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_18 <- read_csv("data/raw/adpml/adpml_exp/loss_2018_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_18 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_19 <- read_csv("data/raw/adpml/adpml_exp/loss_2019_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_19 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_20 <- read_csv("data/raw/adpml/adpml_exp/loss_2020_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_20 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_21 <- read_csv("data/raw/adpml/adpml_exp/loss_2021_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_21 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 loss_22 <- read_csv("data/raw/adpml/adpml_exp/loss_2022_adpml_buffers.csv") %>% 
   select(-c(".geo")) %>% 
   rename(loss_22 = "sum")%>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 locations <- read_csv("data/raw/adpml/adpml_exp/buffers_with_mean_citydist.csv") %>% 
   select(-c(".geo")) %>%
   mutate(treated = ifelse(str_sub(`system:index`, start = 1, end = 1) == "2", 1, 0))
 
 dat <- forest_2000 %>% 
   left_join(loss_01) %>% 
   left_join(loss_02) %>% 
   left_join(loss_03) %>%     
   left_join(loss_04) %>% 
   left_join(loss_05) %>% 
   left_join(loss_06) %>% 
   left_join(loss_07) %>% 
   left_join(loss_08) %>% 
   left_join(loss_09) %>% 
   left_join(loss_10) %>% 
   left_join(loss_11) %>% 
   left_join(loss_12) %>% 
   left_join(loss_13) %>% 
   left_join(loss_14) %>% 
   left_join(loss_15) %>% 
   left_join(loss_16) %>% 
   left_join(loss_17) %>% 
   left_join(loss_18) %>% 
   left_join(loss_19) %>% 
   left_join(loss_20) %>% 
   left_join(loss_21) %>%
   left_join(loss_22) %>%
   left_join(locations)
 
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
 
 
 save(dat_long, file = "data/processed/adpml/dat_synth.Rdata")
 write.csv(dat_long, "data/processed/adpml/dat_synth.csv")