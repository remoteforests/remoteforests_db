# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.3)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.1, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

source("pw.R")

year <- "" # insert year of remeasurement
area <- "" # insert abbreviation of remeasured area

## CRO 2025
plot.id <- tbl(KELuser, "plot") %>%
  filter(country %in% "Croatia",
         foresttype %in% "beech",
         !is.na(lng), !is.na(lat)) %>%
  collect() %>%
  group_by(plotid) %>%
  arrange(desc(date), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., id) %>% 
  pull(id)

## ROM 2025
plot.id <- tbl(KELuser, "plot") %>%
  filter(stand %in% c("Criva", "Paulic"),
         !is.na(lng), !is.na(lat)) %>%
  collect() %>%
  group_by(plotid) %>%
  arrange(desc(date), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., id) %>% 
  pull(id)

## SLO 2025
plot.id <- tbl(KELuser, "plot") %>%
  filter(location %in% c("Poloniny", "Vihorlat"),
         !date %in% 2022,
         !is.na(lng), !is.na(lat)) %>%
  collect() %>%
  group_by(plotid) %>%
  arrange(desc(date), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., id) %>% 
  pull(id)

## SLO_T 2025
plot.id <- tbl(KELuser, "plot") %>%
  filter(stand %in% c("Kolienec", "Ploska"),
         !is.na(lng), !is.na(lat)) %>%
  collect() %>%
  group_by(plotid) %>%
  arrange(desc(date), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., id) %>% 
  pull(id)

# 1. FORM -----------------------------------------------------------------

# 1. 1. data --------------------------------------------------------------

data.form <- tbl(KELuser, "tree") %>%
  filter(plot_id %in% plot.id,
         !treetype %in% c("m", "x", "t", "g", "r")) %>%
  left_join(., tbl(KELuser, "species_fk") %>% select(species = id, sp_code), by = "species") %>%
  inner_join(., tbl(KELuser, "plot") %>% select(plot_id = id, plotid), by = "plot_id") %>%
  collect() %>%
  mutate(treen = as.numeric(treen),
         #stem = substr(treeid, nchar(treeid), nchar(treeid)), # thermophilic
         mortality = "",
         microsites = "") %>%
  select(plotid, treen, 
         #stem, # thermophilic
         status, growth, layer, species = sp_code, dbh_mm, decay, decay_wood, decayht, mortality, microsites) %>%
  arrange(plotid, treen)

# 1. 2. export ------------------------------------------------------------

wb <- createWorkbook()

for(PL in unique(data.form$plotid)){
  
  header <- data.frame(treen = c("plotid", PL, "","treen"),
                       #stem = c("", "", "", "stem"), # thermophilic
                       status = c("", "", "status", "/new"),
                       growth = c("", "", "growth", "/new"),
                       layer = c("date (d/m/y)", "group", "layer", "/new"),
                       species = c("", "", "", "species"),
                       dbh_mm = c("", "", "dbh_mm", "/new"),
                       decay = c("", "", "decay", "/new"),
                       decay_wood = c("slope", "aspect", "decay_wood", "/new"),
                       decayht = c("", "", "decayht","/new"),
                       mortality = c("landform", "hillform", "", "mortality"),
                       microsites = c("", "", "", "microsites"),
                       stringsAsFactors = F)
  
  addWorksheet(wb, PL)
  writeData(wb, sheet = PL, 
            data.form %>% 
              filter(plotid %in% PL) %>% 
              select(-plotid) %>% 
              arrange(treen) %>% 
              rbind(header, .),
            colNames = F,
            borders = "all")

}

saveWorkbook(wb, paste(year, area, "forms.xlsx", sep = "_"))

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
