# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.3)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.1, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

source("pw.R")

# 1. LOOKUPS --------------------------------------------------------------

# 1. 1. landform ----------------------------------------------------------

landform <- data.frame(id = c(1:5),
                       value = c("1 - top", "2 - concave", "3 - mid", "4 - convex", "5 - bottom"))

# 1. 2. hillform ----------------------------------------------------------

hillform <- data.frame(id = c(1:3),
                       value = c("1 - top", "2 - mid", "3 - bottom"))

# 1. 3. species -----------------------------------------------------------

species <- tbl(KELuser, "tree") %>%
  inner_join(., tbl(KELuser, "species_fk"), by = c("species" = "id")) %>%
  distinct(., treeid, species) %>%
  group_by(species) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(id = row_number()) %>%
  select(id, value = species) %>%
  collect()

# 1. 4. status ------------------------------------------------------------

status <- data.frame(id = c(0, 1),
                     value = c("0 - dead",
                               "1 - alive"))

# 1. 5. integrity ---------------------------------------------------------

integrity <- data.frame(id = c(1, 2, 3, 4, 5),
                     value = c("1 - full",
                               "2 - crown breakage",
                               "3 - stem breakage (tree height >= 1.3 m)",
                               "4 - stump (tree height < 1.3 m)",
                               "5 - uprooted"))

# 1. 6. growth ------------------------------------------------------------

growth <- data.frame(id = c(0:1),
                     value = c("suppressed", "released"))

# 1. 7. layer -------------------------------------------------------------

layer <- data.frame(id = c(11:13),
                    value = c("upper", "mid", "lower"))

# 1. 8. decay -------------------------------------------------------------

decay <- data.frame(id = c(1:5),
                    value = c(1:5))

# 1. 9. decayht -----------------------------------------------------------

decayht <- data.frame(id = c(0:9),
                      value = c("0.0 - 9.9 m",
                                "10.0 - 19.9 m",
                                "20.0 - 29.9 m",
                                "30.0 - 39.9 m",
                                "40.0 - 49.9 m",
                                "50.0 - 59.9 m",
                                "60.0 - 69.9 m",
                                "70.0 - 79.9 m",
                                "80.0 - 89.9 m",
                                "90.0 - 99.9 m"))

# 1. 10. mortality ---------------------------------------------------------

mort <- data.frame(id = c(0, 111:113, 121:123, 131:133, 141:143, 15:17, 21, 31, 411:413, 42, 51, 61, 71),
                   value = c("0 - no clear cause (max 2 trees per plot)",
                             "111 - crown break - wind",
                             "112 - crown break - ice/snow",
                             "113 - crown break - another falling tree",
                             "121 - stem break while alive - wind",
                             "122 - stem break while alive - ice/snow",
                             "123 - stem break while alive - another falling tree",
                             "131 - stem break while dead - wind",
                             "132 - stem break while dead - ice/snow",
                             "133 - stem break while dead - another falling tree",
                             "141 - uprooted - wind",
                             "142 - uprooted - ice/snow",
                             "143 - uprooted - another falling tree",
                             "15 - lightning",
                             "16 - landslide",
                             "17 - avalanche",
                             "21 - competition",
                             "31 - fungi infection (NOT Fomitopsis in spruce stands)",
                             "411 - Ips typographus - bark signs",
                             "412 - Ips typographus - conks of Fomitopsis",
                             "413 - Ips typographus - qualified estimation",
                             "42 - insects (other than Ips typographus)",
                             "51 - no clear cause (3 or more trees per plot)",
                             "61 - significant damage by game",
                             "71 - logging"))

mort.integrity <- bind_rows(
  mort %>% filter(id %in% c(0, 15:17, 21, 31, 411:413, 42, 51, 61)) %>% mutate(integrity = 1) %>% select(integrity, id, value),
  mort %>% filter(id %in% c(111:113, 15:17, 21, 31, 411:413, 42, 61)) %>% mutate(integrity = 2) %>% select(integrity, id, value),
  mort %>% filter(id %in% c(121:123, 131:133, 15:17, 21, 31, 411:413, 42, 61)) %>% mutate(integrity = 3) %>% select(integrity, id, value),
  mort %>% filter(id %in% c(121:123, 131:133, 15:17, 21, 31, 411:413, 42, 61, 71)) %>% mutate(integrity = 4) %>% select(integrity, id, value),
  mort %>% filter(id %in% c(141:143, 15:17, 21, 31, 411:413, 42, 61)) %>% mutate(integrity = 5) %>% select(integrity, id, value)
) 

# 1. 11. microsite --------------------------------------------------------

microsite <- data.frame(id = c(1:47),
                        value = c(1:47))

# 1. 12. yes / no ---------------------------------------------------------

yes_no <- data.frame(id = c(0:1),
                     value = c("no", "yes"))

# 1. 13. export -----------------------------------------------------------

lookups <- list("lookup_landform" = landform,
                "lookup_hillform" = hillform,
                "lookup_species" = species,
                "lookup_status" = status,
                "lookup_integrity" = integrity,
                "lookup_growth" = growth,
                "lookup_layer" = layer,
                "lookup_decay" = decay,
                "lookup_decayht" = decayht,
                "lookup_c_mort" = mort.integrity,
                "lookup_microsite" = microsite,
                "lookup_yes_no" = yes_no) 

write.xlsx(lookups, "lookups.xlsx")

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
