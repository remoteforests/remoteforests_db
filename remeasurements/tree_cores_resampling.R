# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.3)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.1, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

source("pw.R")

# 1. data -----------------------------------------------------------------

plot.id <- tbl(KELuser, "plot") %>%
  filter(plottype %in% 3,
         !is.na(altitude_m)) %>%
  pull(id)

tree.id <- tbl(KELuser, "tree") %>%
  filter(!onplot %in% 0,
         !is.na(x_m),
         !species %in% "99",
         dbh_mm >= 100,
         status %in% 1,
         integrity %in% 1) %>%
  pull(id)

core.id <- tbl(KELuser, "core") %>%
  filter(coretype %in% 1,                                      
         !corestatus %in% c(2, 3),                            
         !crossdated %in% c(12, 21, 22),
         missing_years <= 30 | is.na(missing_years)) %>%
  pull(id)

# 1. 1. 1st census --------------------------------------------------------

data.1c <- tbl(KELuser, "plot") %>%
  filter(id %in% plot.id,
         census %in% 1) %>%
  select(plot_id = id, stand, foresttype, plotid, altitude_m) %>%
  inner_join(., tbl(KELuser, "tree") %>% 
               filter(id %in% tree.id) %>%
               select(tree_id = id, plot_id, treeid, species, dbh_mm),
             by = "plot_id") %>%
  inner_join(., tbl(KELuser, "core") %>% 
               filter(id %in% core.id) %>%
               select(core_id = id, tree_id, missing_years),
             by = "tree_id") %>%
  inner_join(., tbl(KELuser, "species_fk") %>% 
               select(species = id, SPCD = sp_code),
             by = "species") %>%
  inner_join(., tbl(KELuser, "ring") %>%
               group_by(core_id) %>%
               summarise(n = n()),
             by = "core_id") %>%
  collect() %>%
  group_by(stand, foresttype, plotid, altitude_m, treeid, species, SPCD, dbh_mm) %>%
  summarise(age = sum(n, missing_years, na.rm = T)) %>% 
  ungroup()

# 1. 2. recent census -----------------------------------------------------

data.re <- tbl(KELuser, "tree") %>% 
  filter(id %in% tree.id,
         treeid %in% local(data.1c$treeid)) %>%
  inner_join(., tbl(KELuser, "plot") %>%
               filter(id %in% plot.id,
                      census %in% c(2, 3, 4)) %>%
               group_by(plotid) %>% 
               arrange(desc(date), .by_group = T) %>%
               filter(row_number() == 1) %>%
               ungroup(),
             by = c("plot_id" = "id")) %>%
  select(plotid, treeid) %>%
  collect()

# 1. 3. all ---------------------------------------------------------------

data.all <- bind_rows(
    inner_join(data.1c, data.re, by = c("plotid", "treeid")),
    data.1c %>% filter(!plotid %in% local(data.re$plotid))) %>%
  mutate(foresttype_stand = paste(foresttype, stand, sep = "_"))

# 2. sub-sampling ---------------------------------------------------------

# 2. 1. parameters --------------------------------------------------------

no_of_size_age_classes <- 5

## species-specific proportions of trees to be sub-sampled 
## by foresttype + stand + species + dbh-age-elevation class
proportion_PIAB <- 0.15
proportion_FASY <- 0.20
proportion_others <- 0.35

## classify trees when sample size is larger then minimum; otherwise keep all trees	
min_trees_for_kmeans <- 35

## minimum number of trees for sub-sampling a cluster
N_min_cluster_trees <- 15 

# 2. 2. loop --------------------------------------------------------------

stand.list <- unique(data.all$foresttype_stand)  
st.array <- array(list(NULL), dim = length(stand.list))

for(i in 1:length(stand.list)){
  
  st <- stand.list[i]
  st.data <- data.all[data.all$foresttype_stand %in% st, ]
  
  species.list <- unique(st.data$SPCD)
  
  ## loop through each focal species and collect samples
  sp.array <- array(list(NULL), dim = length(species.list))
  
  for(ii in 1:length(species.list)){
    
    sp <- species.list[ii]
    targets <- st.data[st.data$SPCD %in% sp, ]
    
    ## adjust subsample_proportion by taxa
    subsample.proportion <- case_when(
      sp %in% "PIAB" ~ proportion_PIAB,
      sp %in% "FASY" ~ proportion_FASY,
      .default = proportion_others
    )
    
    ## subset
    targets.sub <- targets[, names(targets) %in% c("dbh_mm", "age", "altitude_m")]
    
    ## sub-sample when sample size is larger then minimum	
    if(nrow(targets.sub) > min_trees_for_kmeans){
      
      ## kmeans size-age-elevation clusters
      if(nrow(targets.sub) > 30) {
        
        k <- no_of_size_age_classes
        
      } else {
        
        k <- 4 # use fewer clusters when sample size < 30
      }
      
      kmeans.out <- kmeans(targets.sub, centers = k, nstart = 20)
      targets <- cbind(targets, "cluster" = kmeans.out$cluster)
      
      ## get random sample from each cluster
      cluster.data <- array(list(NULL), dim = k)
      
      ## loop through each cluster and sample
      for(c in 1:k){
        
        ## subset cluster
        class <- targets[targets$cluster %in% c, ]
        
        ## random sample
        if(nrow(class) > N_min_cluster_trees){
          
          set.seed(1)
          keep <- sample(class$treeid, subsample.proportion * length(unique(class$treeid)))
          keep.rows <- which(class$treeid %in% keep)
          targets.random <- class[keep.rows, ]
          
        } else {
          
          targets.random <- class
        }
        
        ## combine
        cluster.data[[c]] <- targets.random
      }
      
      ## make dataframe
      cluster.data.df <- do.call(rbind, cluster.data)
      
    } else {
      
      cluster.data.df <- cbind(targets, "cluster" = 1)
    } # end sub-sampling
    
    ## combine			
    sp.array[[ii]] <- cluster.data.df
  } # species loop
  
  ## list to dataframe
  sp.df <- do.call(rbind, sp.array)
  
  ## combine
  st.array[[i]] <- sp.df
} # foresttype_stand loop

data.sample <- do.call(rbind, st.array)

# 3. export ---------------------------------------------------------------

year <- "" # insert year of remeasurement

## 2026 ALB + BOS + ROM
pid <- tbl(KELuser, "plot") %>%
  filter(stand %in% c("Curraj i Eperm",
                      "Lumi i Gashit",
                      "Perucica",
                      "Bistra valley",
                      "Cajmrsk",
                      "Cocos-Dragus",
                      "Giumalau"),
         foresttype %in% c("beech", "spruce"),
         !plottype %in% 11,
         !census %in% 1,
         !is.na(lng), !is.na(lat)) %>%
  distinct(., plotid) %>% 
  pull(plotid)

write.xlsx(
  data.sample %>% 
    filter(plotid %in% pid) %>%
    select(stand, foresttype, plotid, treeid, species, dbh_mm, age) %>%
    arrange(treeid),
  paste(year, "tree_cores_resampling.xlsx", sep = "_"))

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
