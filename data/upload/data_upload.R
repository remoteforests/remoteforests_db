# setup -------------------------------------------------------------------

# R 3.6.3 (2020-02-29)

library(openxlsx) # 4.1.4
library(pool) # 0.1.4.3
library(RPostgreSQL) # 0.6-2 (DBI 1.1.0)
library(tidyverse) # 1.3.0 (dplyr 1.0.7, forcats 0.5.0, ggplot2 3.3.5, purr 0.3.4, readr 1.3.1, stringr 1.4.0, tibble 3.0.0, tidyr 1.0.2)
library(zoo) # 1.8-7

source("pw.R")

source("data/upload/data_upload_fc.R")

data.list <- list()

# STRUCTURAL DATA ---------------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2023/clean")

# 1. reading --------------------------------------------------------------

data.list$plot <- read_data("plot")
data.list$tree <- read_data("tree")
data.list$mortality <- read_data("mortality")
data.list$microsites <- read_data("microsites")
data.list$tree_quality <- read_data("tree_quality")
data.list$deadwood <- read_data("deadwood")
data.list$regeneration <- read_data("regeneration")
data.list$regeneration_subplot <- read_data("regeneration_subplot")
data.list$reg_subplot_position <- read_data("reg_subplot_position")
data.list$soil_profile <- read_data("soil")
data.list$vegetation <- read_data("vegetation")
data.list$habitat_signs <- read_data("habitat")

# 2. preparing & uploading ------------------------------------------------

## plot

data.list$plot <- prepare_data("plot")

upload_data("plot")

## plot_id

data.list$tree <- prepare_data("tree")
data.list$deadwood <- prepare_data("deadwood")
data.list$regeneration <- prepare_data("regeneration")
data.list$regeneration_subplot <- prepare_data("regeneration_subplot")
data.list$reg_subplot_position <- prepare_data("reg_subplot_position")
data.list$soil_profile <- prepare_data("soil_profile")
data.list$vegetation <- prepare_data("vegetation")
data.list$habitat_signs <- prepare_data("habitat_signs")

upload_data(x = c("tree", "deadwood", "regeneration", "regeneration_subplot", "reg_subplot_position", "soil_profile", "vegetation", "habitat_signs"))

## tree_id

data.list$mortality <- prepare_data("mortality")
data.list$microsites <- prepare_data("microsites")
data.list$tree_quality <- prepare_data("tree_quality")

upload_data(x = c("mortality", "microsites", "tree_quality"))

# DENDROCHRONOLOGICAL DATA ------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Downloads")

# 1. reading --------------------------------------------------------------

data.list$core <- read_data("core")
data.list$ring <- read_data("ring")

# 2. preparing & uploading ------------------------------------------------

## tree_id

data.list$core <- prepare_data("core")

upload_data("core")

## core_id

data.list$ring <- prepare_data("ring")

upload_data("ring")

# DISTURBANCE DATA --------------------------------------------------------

# 1. reading --------------------------------------------------------------

data.list$dist_tree <- data.event
data.list$dist_plot <- data.kde.all %>% distinct(., plotid, type, year_min, year_max, ncores)
data.list$dist_chrono <- data.kde.all %>% select(plotid, type, year, ca_pct, kde)
data.list$dist_event <- data.peaks.all %>% select(plotid, type, year)
data.list$dist_stand <- dist.stand

# 2. preparing & uploading ------------------------------------------------

## dist_tree

data.list$dist_tree <- prepare_data("dist_tree")

upload_data("dist_tree")

## dist_plot

data.list$dist_plot <- prepare_data("dist_plot")

upload_data("dist_plot")

## dist_chrono

data.list$dist_chrono <- prepare_data("dist_chrono")

upload_data("dist_chrono")

## dist_event

data.list$dist_event <- prepare_data("dist_event")

upload_data("dist_event")

## dist_stand

data.list$dist_stand <- prepare_data("dist_stand")

upload_data("dist_stand")

# PARAMETERS --------------------------------------------------------------

# 1. reading --------------------------------------------------------------

data.list$parameters_plot <- data.col

# 2. preparing & uploading ------------------------------------------------

data.list$parameters_plot <- prepare_data("parameters_plot")

upload_data("parameters_plot")

# WINSCANOPY --------------------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Downloads")

# 1. reading --------------------------------------------------------------

# ## raw data
# 
# canopy.list <- list.files(pattern = "*raw.txt", recursive = F)
# 
# for (i in canopy.list) {
# 
#   can.data <- read_delim(i, delim = "\t", skip = 6, col_names = F)
# 
#   can.names <- read_delim(i, delim = "\t", skip = 1, n_max = 1, col_names = F) %>%
#     select(1:ncol(can.data))
# 
#   colnames(can.data) <- can.names[1,]
# 
#   write.xlsx(x = can.data, file = paste0(substr(i, 1, nchar(i) - 7), "clean.xlsx"), row.names = F)
#
# }
# 
# ### The structure of the raw data needs to be checked and adjusted manually.

canopy.list <- list.files(pattern = "*clean.xlsx", recursive = F)

canopy.df <- tibble()

for (i in canopy.list) {
  
  can.new <- read.xlsx(i, sheet = 2)
  
  canopy.df <- bind_rows(canopy.df, can.new)
  
}

data.list$canopy_analysis <- canopy.df %>%
  gather(., parameter, value, 3:15) %>%
  mutate(plotid = substr(file, 1, nchar(file) - 6),
         transect = substr(file, nchar(file) - 4, nchar(file) - 4),
    date = substr(date, 1, 4),
    date = as.numeric(date))
    
# 2. preparing & uploading ------------------------------------------------

data.list$canopy_analysis <- prepare_data("canopy_analysis")

upload_data("canopy_analysis")

# DEADWOOD POSITIONS ------------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Downloads")

# 1. reading --------------------------------------------------------------

### Sheet name, date and plot codes need to be adjusted; column names, species and decay codes checked. 

data.list$deadwood_tree <- read.xlsx(".xlsx", sheet = "") %>%
  select(plotid = , object_id = , species = , decay = , length_m = , volume_m3 = ) %>%
  mutate(date = ,
  plotid = case_when(
    plotid  %in%  ~ "",
    plotid  %in%  ~ ""),
  species = case_when(
    species %in% 100 ~ "99",
    species %in% 200 ~ "Picea abies",
    species %in% 300 ~ "Fagus sylvatica",
    species %in% 400 ~ "Abies alba",
    species %in% 500 ~ "Acer pseudoplatanus",
    species %in% 600 ~ "Acer platanoides",
    species %in% 700 ~ "Fraxinus excelsior",
    species %in% 800 ~ "Sorbus aucuparia",
    species %in% 900 ~ "Ulmus",
    TRUE ~ "99"),
  decay = case_when(
    decay %in% 100 ~ 1,
    decay %in% 200 ~ 2,
    decay %in% 300 ~ 3,
    decay %in% 400 ~ 4,
    decay %in% 500 ~ 5,
    TRUE ~ 99),
  length_m = round(length_m, 2),
  volume_m3 = round(volume_m3, 5)) %>%
  filter(!plotid %in% NA)

data.list$deadwood_position <- read.xlsx(".xlsx", sheet = "") %>%
  select(plotid = , object_id = , end_number = , diameter_mm = , x_m = , y_m = , z_m = ) %>%
  mutate(date = ,
    plotid = case_when(
      plotid  %in%  ~ "",
      plotid  %in%  ~ "")) %>% 
  filter(!plotid %in% NA) %>%
  distinct(., .keep_all = T)

# 2. preparing & uploading ------------------------------------------------

data.list$deadwood_tree <- prepare_data("deadwood_tree")
data.list$deadwood_position <- prepare_data("deadwood_position")

data.list$deadwood_tree <- data.list$deadwood_tree %>% select(colorder("deadwood_tree"))

upload_data(x = c("deadwood_tree", "deadwood_position"))

# disconnection -----------------------------------------------------------

poolClose(KELuser)
poolClose(KELadmin)

