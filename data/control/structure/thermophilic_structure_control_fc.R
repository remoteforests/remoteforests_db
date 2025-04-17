read_structural_data <- function(file){
  #' @description read structural data from rewritten forms (.xlsx)
  #' @param file path to the .xlsx file
  
  data.list <- list()
  
  # plot
  
  data.list$plot <- read.xlsx(file, sheet = "plot")
  
  if(!identical(c("date", "plotid", "census",	"country", "location", "stand",
                  "plot",	"subplot", "lng", "lat", "plotsize", "dbh_min", "plottype",
                  "foresttype", "slope", "aspect", "landform", "hillform", "ownership"), 
                names(data.list$plot))) 
    
    stop("Plot data do not match with required table format.")
  
  data.list$plot <- data.list$plot %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           census = as.numeric(census),
           country = as.character(country),
           location = as.character(location),
           stand = as.character(stand),
           plot = as.character(plot),
           subplot = as.numeric(subplot),
           lng = as.numeric(lng),
           lat = as.numeric(lat),
           plotsize = as.numeric(plotsize),
           dbh_min = as.numeric(dbh_min),
           plottype = as.numeric(plottype),
           foresttype = as.character(foresttype),
           slope = as.numeric(slope),
           aspect = as.numeric(aspect),
           landform = as.numeric(landform),
           hillform = as.numeric(hillform),
           ownership = as.numeric(ownership))
  
  # tree
  
  data.list$tree <- read.xlsx(file, sheet = "tree")
  
  if(!identical(c("date", "plotid", "treen", "stem", "treetype", "x_m",	"y_m",
                  "status", "growth", "layer", "species", "dbh_mm", "height_m",
                  "crownht_m", "crowndiam1_m", "crowndiam2_m", "decay", 
                  "decay_wood", "decayht"),
                names(data.list$tree)))
    
    stop("Tree data do not match with required table format.")
  
  data.list$tree <- data.list$tree %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           treen = as.character(treen),
           stem = as.character(stem),
           treetype = as.character(treetype),
           x_m = as.numeric(x_m),
           y_m = as.numeric(y_m),
           status = as.numeric(status),
           growth = as.numeric(growth),
           layer = as.numeric(layer),
           species = as.character(species),
           dbh_mm = as.numeric(dbh_mm),
           height_m = as.numeric(height_m),
           crownht_m = as.numeric(crownht_m),
           crowndiam1_m = as.numeric(crowndiam1_m),
           crowndiam2_m = as.numeric(crowndiam2_m),
           decay = as.numeric(decay),
           decay_wood = as.numeric(decay_wood),
           decayht = as.numeric(decayht))
  
  # microsites
  
  data.list$microsites <- read.xlsx(file, sheet = "microsites")
  
  if(!identical(c("date", "plotid", "treen", "stem", "microsite", "count"),
                names(data.list$microsites)))
    
    stop("Microsites data do not match with required table format.")
  
  data.list$microsites <- data.list$microsites %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           treen = as.character(treen),
           treen = case_when(
             nchar(treen) == 1 ~ paste0("00", treen),
             nchar(treen) == 2 ~ paste0("0", treen),
             nchar(treen) == 3 ~ treen),
           stem = as.character(stem),
           treeid = paste(plotid, treen, stem, sep = "_"),
           microsite = as.numeric(microsite),
           count = as.numeric(count)) %>%
    select(date, treeid, microsite, count)
  
  # mortality
  
  data.list$mortality <- read.xlsx(file, sheet = "mortality")
  
  if(!identical(c("date", "plotid", "treen", "stem", "mort_agent"),
                names(data.list$mortality)))
    
    stop("Mortality data do not match with required table format.")
  
  data.list$mortality <- data.list$mortality %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           treen = as.character(treen),
           treen = case_when(
             nchar(treen) == 1 ~ paste0("00", treen),
             nchar(treen) == 2 ~ paste0("0", treen),
             nchar(treen) == 3 ~ treen),
           stem = as.character(stem),
           treeid = paste(plotid, treen, stem, sep = "_"),
           mort_agent = as.numeric(mort_agent)) %>%
    select(date, treeid, mort_agent)
  
  # deadwood
  
  data.list$deadwood <- read.xlsx(file, sheet = "deadwood")
  
  if(!identical(c("date", "plotid", "transect",	"transect_length_m", "species", "dbh_mm", "decay"),
                names(data.list$deadwood)))
    
    stop("Deadwood data do not match with required table format.")
  
  data.list$deadwood <- data.list$deadwood %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           transect = as.numeric(transect),
           transect_length_m = as.numeric(transect_length_m),
           species = as.character(species),
           dbh_mm = as.numeric(dbh_mm),
           decay = as.numeric(decay))
  
  # regeneration
  
  data.list$regeneration <- read.xlsx(file, sheet = "regeneration")

  if(!identical(c("date", "plotid", "species", "htclass", "browsing", "regeneratedon", "count"),
                names(data.list$regeneration)))

    stop("Regeneration data do not match with required table format.")

  data.list$regeneration <- data.list$regeneration %>%
    select(-browsing) %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           species = as.character(species),
           htclass = as.numeric(htclass),
           regeneratedon = as.numeric(regeneratedon),
           count = as.numeric(count))
  
  # regeneration_subplot
  
  data.list$regeneration_subplot <- read.xlsx(file, sheet = "regeneration_subplot")
  
  if(!identical(c("date", "plotid", "subplot_n", "subplotsize_m2", "species", "htclass",
                  "browsing",	"regeneratedon", "count"), 
                names(data.list$regeneration_subplot))) 
    
    stop("Regeneration_subplot data do not match with required table format.")
  
  data.list$regeneration_subplot <- data.list$regeneration_subplot %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           subplot_n = as.numeric(subplot_n),
           subplotsize_m2 = as.numeric(subplotsize_m2),
           species = as.character(species),
           htclass = as.numeric(htclass),
           browsing = as.numeric(browsing),
           regeneratedon = as.numeric(regeneratedon),
           count = as.numeric(count))
  
  # soil
  
  # data.list$soil <- read.xlsx(file, sheet = "soil_profile")
  # 
  # if(!identical(c("date", "plotid", "sample",	"soil_horizon", "depth_cm"), 
  #               names(data.list$soil))) 
  #   
  #   stop("Soil data do not match with required table format.")
  # 
  # data.list$soil <- data.list$soil %>%
  #   mutate(date = as.numeric(date),
  #          plotid = as.character(plotid),
  #          sample = as.numeric(sample),
  #          soil_horizon = as.character(soil_horizon),
  #          depth_cm = as.numeric(depth_cm))
  
  # vegetation
  
  # data.list$vegetation <- read.xlsx(file, sheet = "vegetation", detectDates = T)
  # 
  # if(!identical(c("date", "sampling_date", "plotid", "large_gap",	"vegetationht", 
  #                 "vegetation_cover", "vaccinium_myrtillus_per", "rubus_per", 
  #                 "bryopsida_per", "polypodiopsida_per", "poaceae_per", "ericaceae_per",
  #                 "other_per", "biotope_quality", "biotope_trend", "large_herbivore_feces"), 
  #               names(data.list$vegetation))) 
  #   
  #   stop("Vegetation data do not match with required table format.")
  # 
  #  data.list$vegetation <- data.list$vegetation %>%
  #    mutate(date = as.numeric(date),
  #           sampling_date = as.POSIXlt(sampling_date, tz = "UTC", format = "%Y-%m-%d"),
  #           plotid = as.character(plotid),
  #           large_gap = as.numeric(large_gap),
  #           vegetationht = as.numeric(vegetationht),
  #           vegetation_cover = as.numeric(vegetation_cover),
  #           vaccinium_myrtillus_per = as.numeric(vaccinium_myrtillus_per),
  #           rubus_per = as.numeric(rubus_per),
  #           bryopsida_per = as.numeric(bryopsida_per),
  #           polypodiopsida_per = as.numeric(polypodiopsida_per),
  #           poaceae_per = as.numeric(poaceae_per),
  #           ericaceae_per = as.numeric(ericaceae_per),
  #           other_per = as.numeric(other_per),
  #           biotope_quality = as.numeric(biotope_quality),
  #           biotope_trend = as.numeric(biotope_trend),
  #           large_herbivore_feces = as.numeric(large_herbivore_feces))
  
  # habitat
  
  # data.list$habitat <- read.xlsx(file, sheet = "habitat", detectDates = T)
  # 
  # if(!identical(c("date", "sampling_date", "plotid",	"animal_species", "gender", "habitat_sign_type"), 
  #               names(data.list$habitat))) 
  #   
  #   stop("Habitat data do not match with required table format.")
  # 
  # data.list$habitat <- data.list$habitat %>%
  #   mutate(date = as.numeric(date),
  #          sampling_date = as.POSIXlt(sampling_date, tz = "UTC", format = "%Y-%m-%d"),
  #          plotid = as.character(plotid),
  #          animal_species = as.character(animal_species),
  #          gender = as.numeric(gender),
  #          habitat_sign_type = as.numeric(habitat_sign_type))
  
  return(data.list)
}

check_structural_data <- function(data, fk) {
  #' @description check structural data for possible errors
  #' @param data list of structural datasets (data.raw)
  #' @param fk list of fk encodings extracted from database
  
  error.list <- list()
  
  plot.check <- tbl(KELuser, "plot") %>% 
    filter(id %in% old.plot.id) %>%
    select(plotid, date_old = date, country_old = country, location_old = location,
           stand_old = stand, lng_old = lng, lat_old = lat, plotsize_old = plotsize,
           dbh_min_old = dbh_min, plottype_old = plottype, foresttype_old = foresttype) %>%
    collect() %>%
    right_join(., data$plot, by = "plotid")
  
  tree.check <- tbl(KELuser, "tree") %>%
    filter(plot_id %in% old.plot.id) %>%
    select(treeid, status_old = status, dbh_old = dbh_mm) %>%
    collect() %>%
    right_join(., data$tree, by = "treeid")
  
  # plot
  
  error.list$P_date <- plot.check %>% filter(is.na(date) | !date > date_old)
  error.list$P_plotid <- anti_join(data$plot, tbl(KELuser, "plot") %>% distinct(., plotid) %>% collect(), by = "plotid")
  error.list$P_census <- data$plot %>% filter(!census %in% fk$plot_census_fk)
  error.list$P_country <- data$plot %>% filter(!country %in% fk$country_fk)
  error.list$P_country_change <- plot.check %>% rowwise() %>% filter(!country %in% country_old)
  error.list$P_location <- data$plot %>% filter(!location %in% fk$location_fk)
  error.list$P_location_change <- plot.check %>% rowwise() %>% filter(!location %in% location_old)
  error.list$P_stand <- plot.check %>% rowwise() %>% filter(!stand %in% stand_old)
  error.list$P_lng <- plot.check %>% rowwise() %>% filter(!lng %in% lng_old)
  error.list$P_lat <- plot.check %>% rowwise() %>% filter(!lat %in% lat_old)
  error.list$P_plotsize <- data$plot %>% filter(!plotsize %in% c(500, 1000, 1500))
  error.list$P_plotsize_change <- plot.check %>% rowwise() %>% filter(!plotsize %in% plotsize_old)
  error.list$P_dbh_min <- data$plot %>% filter(!dbh_min %in% c(50, 60, 100))
  error.list$P_dbh_min_change <- plot.check %>% rowwise() %>% filter(!dbh_min %in% dbh_min_old)
  error.list$P_plottype <- data$plot %>% filter(!plottype %in% fk$plottype_fk)
  error.list$P_plottype_change <- plot.check %>% rowwise() %>% filter(!plottype %in% plottype_old)
  error.list$P_foresttype <- data$plot %>% filter(!foresttype %in% fk$foresttype_fk)
  error.list$P_foresttype_change <- plot.check %>% rowwise() %>% filter(!foresttype %in% foresttype_old)
  error.list$P_slope <- data$plot %>% filter(!slope %in% c(0:90))
  error.list$P_aspect <- data$plot %>% filter(!aspect %in% c(0:360))
  error.list$P_landform <- data$plot %>% filter(!landform %in% c(1:5))
  error.list$P_hillform <- data$plot %>% filter(!hillform %in% c(1:3))
  error.list$P_ownership <- anti_join(data$plot, tbl(KELuser, "person") %>% distinct(., id) %>% collect(), by = c("ownership" = "id"))
  error.list$P_duplicates <- as_tibble(duplicated(data$plot)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$plot %>% rownames_to_column("id"), by = "id")
  error.list$P_ak <- data$plot %>% group_by(date, plotid) %>% filter(n() > 1)
  
  # tree
  
  error.list$T_not_in_plot <- anti_join(data$tree, data$plot, by = c("date", "plotid"))
  error.list$T_treetype <- data$tree %>% filter(!treetype %in% fk$treetype_fk)
  error.list$T_status <- data$tree %>% filter(!status %in% fk$status_fk)
  error.list$T_growth <- data$tree %>% filter(!growth %in% fk$growth_fk)
  error.list$T_growth_alive <- data$tree %>% filter(status %in% c(1:4) & growth %in% -1)
  error.list$T_growth_dead <- data$tree %>% filter(!status %in% c(1:4) & !growth %in% -1)
  error.list$T_layer <- data$tree %>% filter(!layer %in% fk$layer_fk)
  error.list$T_layer_alive <- data$tree %>% filter(status %in% c(1:4) & layer %in% -1)
  error.list$T_layer_dead <- data$tree %>% filter(!status %in% c(1:4) & !layer %in% -1)
  error.list$T_species <- data$tree %>% filter(!species %in% fk$species_fk)
  error.list$T_dbh <- data$tree %>% filter(dbh_mm < unique(data$plot$dbh_min))
  error.list$T_dbh_alive <- tree.check %>% filter(status %in% c(1:4) & dbh_mm < dbh_old) 
  error.list$T_dbh_dead <- tree.check %>% filter(!status %in% c(1:4) & !status_old %in% c(1:4) & dbh_mm > dbh_old)
  error.list$T_height <- data$tree %>% filter(crownht_m >= height_m)
  error.list$T_decayht_height <- data$tree %>% filter(!status %in% c(1:4) & !is.na(height_m))
  error.list$T_decayht <- data$tree %>% filter(!decayht %in% fk$decayheight_fk)
  error.list$T_decayht_alive <- data$tree %>% filter(status %in% c(1:4) & !decayht %in% -1)
  error.list$T_decayht_dead <- data$tree %>% filter(!status %in% c(1:4) & decayht %in% -1)
  error.list$T_decayht_stump <- data$tree %>% filter(status %in% c(0, 10) & !decayht %in% 0)
  error.list$T_decayht_decay <- data$tree %>% filter(decay %in% 5 & !decayht %in% 0)
  error.list$T_decay_wood <- data$tree %>% filter(!decay_wood %in% fk$decay_wood_fk)
  error.list$T_decay_wood_alive <- data$tree %>% filter(status %in% c(1:4) & !decay_wood %in% -1)
  error.list$T_decay_wood_dead <- data$tree %>% filter(!status %in% c(1:4) & decay_wood %in% -1)
  error.list$T_decay <- data$tree %>% filter(!decay %in% fk$decay_fk)
  error.list$T_decay_alive <- data$tree %>% filter(status %in% c(1:4) & !decay %in% -1)
  error.list$T_decay_dead <- data$tree %>% filter(!status %in% c(1:4) & decay %in% -1)
  error.list$T_decay_stump <- data$tree %>% filter(status %in% c(0, 10) & !decay %in% 5)
  error.list$T_duplicates <- as_tibble(duplicated(data$tree)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$tree %>% rownames_to_column("id"), by = "id")
  error.list$T_ak <- data$tree %>% group_by(date, plotid, treeid) %>% filter(n() > 1)
  
  # mortality
  
  error.list$Mo_not_in_tree <- anti_join(data$mortality, data$tree, by = c("date", "treeid"))
  error.list$Mo_NA <- left_join(tree.check, data$mortality, by = c("date", "treeid")) %>%
    filter(status_old %in% c(1:4) & !status %in% c(1:4) & is.na(mort_agent))
  error.list$Mo_alive <- data$tree %>% filter(status %in% c(1:4)) %>% inner_join(., data$mortality, by = c("date", "treeid"))
  error.list$Mo_dead <- tree.check %>% filter(!status %in% c(1:4) & !status_old %in% c(1:4)) %>% inner_join(., data$mortality, by = c("date", "treeid"))
  error.list$Mo_mort_agent <- data$mortality %>% filter(!mort_agent %in% fk$mort_agent_fk)
  error.list$Mo_0_51 <- data$mortality %>% filter(mort_agent %in% c(0, 51)) %>%
    mutate(plotid = substr(treeid, 1, nchar(treeid) - 6)) %>%
    group_by(plotid, mort_agent) %>%
    filter((mort_agent %in% 0 & n() > 2) | (mort_agent %in% 51 & n() < 3))
  error.list$Mo_duplicates <- as_tibble(duplicated(data$mortality)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$mortality %>% rownames_to_column("id"), by = "id")
  error.list$Mo_ak <- data$mortality %>% group_by(date, treeid, mort_agent) %>% filter(n() > 1)
  
  # microsites
  
  error.list$Mi_not_in_tree <- anti_join(data$microsites, data$tree, by = c("date", "treeid"))
  error.list$Mi_microsite <- data$microsites %>% filter(!microsite %in% fk$microsite_fk)
  error.list$Mi_count <- data$microsites %>% filter(is.na(count) | count < 1)
  error.list$Mi_countable <- data$microsites %>% filter(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47) & count > 1)
  error.list$Mi_duplicates <- as_tibble(duplicated(data$microsites)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$microsites %>% rownames_to_column("id"), by = "id")
  error.list$Mi_ak <- data$microsites %>% group_by(date, treeid, microsite) %>% filter(n() > 1)
  
  # deadwood
  
  error.list$D_not_in_plot <- anti_join(data$deadwood, data$plot, by = c("date", "plotid"))
  error.list$D_transect <- data$deadwood %>% filter(!transect %in% fk$transect_fk)
  error.list$D_transect_length <- data$deadwood %>% filter(!transect_length_m %in% 20)
  error.list$D_species <- data$deadwood %>% filter(!species %in% fk$species_fk)
  error.list$D_dbh_mm <- data$deadwood %>% filter(is.na(dbh_mm))
  error.list$D_dbh_min <- data$deadwood %>% filter(dbh_mm < 50)
  error.list$D_decay <- data$deadwood %>% filter(!decay %in% fk$decay_wood_fk)
  error.list$D_duplicates <- as_tibble(duplicated(data$deadwood)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$deadwood %>% rownames_to_column("id"), by = "id")
  error.list$D_ak <- data$deadwood %>% group_by(date, plotid, transect, species, dbh_mm, decay) %>% filter(n() > 1)
  
  # regeneration
  
  error.list$R_not_in_plot <- anti_join(data$regeneration, data$plot, by = c("date", "plotid"))
  error.list$R_species <- data$regeneration %>% filter(!species %in% fk$species_fk)
  error.list$R_htclass <- data$regeneration %>% filter(!htclass %in% fk$htclass_fk)
  error.list$R_regeneratedon <- data$regeneration %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  error.list$R_count <- data$regeneration %>% filter(is.na(count) | count < 1)
  error.list$R_duplicates <- as_tibble(duplicated(data$regeneration)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$regeneration %>% rownames_to_column("id"), by = "id")
  error.list$R_ak <- data$regeneration %>% group_by(date, plotid, species, htclass, regeneratedon) %>% filter(n() > 1)
  
  # regeneration_subplot
  
  error.list$RS_not_in_plot <- anti_join(data$regeneration_subplot, data$plot, by = c("date", "plotid"))
  error.list$RS_subplot_n <- data$regeneration_subplot %>% filter(!subplot_n %in% fk$transect_fk)
  error.list$RS_subplotsize_m2 <- data$regeneration_subplot %>% filter(!subplotsize_m2 %in% 4)
  error.list$RS_species <- data$regeneration_subplot %>% filter(!species %in% fk$species_fk)
  error.list$RS_htclass <- data$regeneration_subplot %>% filter(!htclass %in% fk$htclass_fk)
  error.list$RS_browsing <- data$regeneration_subplot %>% filter(!browsing %in% fk$browsing_fk)
  error.list$RS_regeneratedon <- data$regeneration_subplot %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  error.list$RS_count <- data$regeneration_subplot %>% filter(is.na(count) | count < 1)
  error.list$RS_duplicates <- as_tibble(duplicated(data$regeneration_subplot)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$regeneration_subplot %>% rownames_to_column("id"), by = "id")
  error.list$RS_ak <- data$regeneration_subplot %>% group_by(date, plotid, subplot_n, species, htclass, regeneratedon, browsing) %>% filter(n() > 1)
  
  # soil
  
  # error.list$S_not_in_plot <- anti_join(data$soil, data$plot, by = c("date", "plotid"))
  # error.list$S_sample <- data$soil %>% filter(!sample %in% c(1:5))
  # error.list$S_soil_horizon <- data$soil %>% filter(!soil_horizon %in% fk$soil_horizon_fk)
  # error.list$S_bedrock <- data$soil %>% mutate(n = ifelse(soil_horizon %in% "R", 1, 0)) %>% 
  #   group_by(date, plotid, sample) %>% summarise(n = sum(n)) %>% filter(!n %in% 1)
  # error.list$S_bedrock_depth <- data$soil %>% filter(soil_horizon %in% "R" & !depth_cm %in% c(-1, 0, 1))
  # error.list$S_depth_cm <- data$soil %>% filter(!soil_horizon %in% "R") %>% filter(is.na(depth_cm) | depth_cm <= 0)
  # error.list$S_duplicates <- as_tibble(duplicated(data$soil)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
  #   inner_join(., data$soil %>% rownames_to_column("id"), by = "id")
  # error.list$S_ak <- data$soil %>% group_by(date, plotid, sample, soil_horizon) %>% filter(n() > 1)
  
  # vegetation
  
  # error.list$V_not_in_plot <- anti_join(data$vegetation, data$plot, by = c("date", "plotid"))
  # error.list$V_sampling_date <- data$vegetation %>% filter(is.na(sampling_date))
  # error.list$V_sampling_date_date <- data$vegetation %>% mutate(sampling = as.numeric(substr(sampling_date, 1, 4))) %>% rowwise() %>% filter(!sampling %in% date)
  # error.list$V_large_gap <- data$vegetation %>% filter(!large_gap %in% fk$large_gap_fk)
  # error.list$V_vegetationht <- data$vegetation %>% filter(!vegetationht %in% fk$vegetationheight_fk)
  # error.list$V_vegetation_cover <- data$vegetation %>% select(-sampling_date) %>%
  #   mutate(cover = vaccinium_myrtillus_per + rubus_per + bryopsida_per + polypodiopsida_per + poaceae_per + ericaceae_per + other_per) %>%
  #   gather(., family, value, vaccinium_myrtillus_per, rubus_per, bryopsida_per, polypodiopsida_per, poaceae_per, ericaceae_per, other_per) %>%
  #   group_by(plotid, vegetation_cover, cover) %>%
  #   summarise(value = max(value)) %>%
  #   filter(vegetation_cover > cover | vegetation_cover < value)
  # error.list$V_biotope_quality <- data$vegetation %>% filter(!biotope_quality %in% fk$biotope_quality_fk)
  # error.list$V_biotope_trend <- data$vegetation %>% filter(!biotope_trend %in% fk$biotope_trend_fk)
  # error.list$V_large_herbivore_feces <- data$vegetation %>% filter(large_herbivore_feces < 0)
  # error.list$V_duplicates <- as_tibble(duplicated(data$vegetation)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
  #   inner_join(., data$vegetation %>% rownames_to_column("id"), by = "id")
  # error.list$V_ak <- data$vegetation %>% group_by(date, plotid, sampling_date) %>% filter(n() > 1)
  
  # habitat
  
  # error.list$H_not_in_plot <- anti_join(data$habitat, data$plot, by = c("date", "plotid"))
  # error.list$H_sampling_date <- data$habitat %>% filter(is.na(sampling_date))
  # error.list$H_sampling_date_date <- data$habitat %>% mutate(sampling = as.numeric(substr(sampling_date, 1, 4))) %>% rowwise() %>% filter(!sampling %in% date)
  # error.list$H_animal_species <- data$habitat %>% filter(!animal_species %in% fk$animal_species_fk)
  # error.list$H_gender <- data$habitat %>% filter(!gender %in% fk$gender_fk)
  # error.list$H_habitat_sign_type <- data$habitat %>% filter(!habitat_sign_type %in% fk$habitat_sign_type_fk)
  # error.list$H_duplicates <- as_tibble(duplicated(data$habitat)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
  #   inner_join(., data$habitat %>% rownames_to_column("id"), by = "id")
  # error.list$H_ak <- data$habitat %>% group_by(date, plotid, sampling_date, animal_species, gender, habitat_sign_type) %>% filter(n() > 1)
  
  return(error.list)
}

circleFun <- function(r){
  #' @description create circle with X and Y coordinates
  #' @param r circle radius
  
  tt <- seq(0, 2 * pi, length.out = 100)
  xx <- r * cos(tt)
  yy <- r * sin(tt)
  
  return(data.frame(X = xx, Y = yy))
}

plotTree <- function(PL){
  #' @description create plot map with tree position, size, status, and species
  #' @param PL unique plotid
  
  data.gg <- data.map %>% filter(plotid %in% PL)
  
  ggplot(data.gg) +
    geom_point(aes(x_m, y_m, 
                   size = dbh_mm, 
                   shape = status,
                   color = species)) +
    scale_size_continuous("DBH (mm)", 
                          limits = c(0, 1400),
                          breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400),
                          range = c(2, 9)) +
    scale_shape_manual("Status", values = c("dead" = 17,
                                            "alive" = 19,
                                            "99" = 18),
                       drop = FALSE) +
    scale_color_manual("Species", values = c("Picea abies" = "lightgreen",
                                             "Fagus sylvatica" = "lightblue",
                                             "Abies alba" = "darkgreen",
                                             "Acer" = "orange",
                                             "Sorbus" =  "darkblue",
                                             "Fraxinus" = "turquoise",
                                             "Quercus" = "yellow",
                                             "Ostrya carpinifolia" = "violet", 
                                             "Pinus" = "brown",
                                             "Others" = "grey"),
                       drop = FALSE) +
    annotate("point", x = 0, y = 0, shape = 3, color = "red", size = 3) +
    geom_path(data = circleFun(r = 7.99), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    geom_path(data = circleFun(r = 12.62), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    geom_path(data = circleFun(r = 17.84), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    geom_path(data = circleFun(r = 21.85), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    theme_bw() +
    geom_text(aes(x_m + 0.5, y_m + 0.5, label = treen), size = 3, color = "grey20") +
    ggtitle(PL)
}

clean_structural_data <- function(data){
  #' @description clean and prepare structural data
  #' @param data list of structural datasets (data.raw)
  
  data.clean <- list()
  
  # plot
  
  data.clean$plot <- tbl(KELuser, "plot") %>% 
    filter(id %in% old.plot.id) %>%
    select(plotid, lng_old = lng, lat_old = lat, plotsize_old = plotsize) %>%
    collect() %>%
    right_join(., data$plot, by = "plotid") %>%
    mutate(census = case_when(
             is.na(plotsize_old) ~ 1,
             !is.na(plotsize_old) & plotsize %in% plotsize_old  ~ 2,
             !is.na(plotsize_old) & plotsize > plotsize_old ~ 3,
             !is.na(plotsize_old) & plotsize < plotsize_old ~ 4),
           lng = ifelse(is.na(lng), lng_old, lng),
           lat = ifelse(is.na(lat), lat_old, lat),
           altitude_m = NA,
           slope = round(slope, 0),
           aspect = round(aspect, 0)) %>%
    select(-lng_old, -lat_old, -plotsize_old)
  
  DEM <- rast("C:/Users/vosta/Desktop/KEL/db/data/external/dem/aster/Europe/DEM_Aster_Europe.tif")
  
  for (i in data.clean$plot$plotid) {
    
    coord <- data.clean$plot %>% filter(plotid %in% i) %>% select(lng, lat)
    
    data.clean$plot <- data.clean$plot %>% 
      mutate(altitude_m = ifelse(plotid %in% i, 
                                 round(extract(DEM, coord, method = "bilinear")[1,2], 0),
                                 altitude_m))
    
    remove(coord)
  }
  
  # mortality
  
  data.clean$mortality <- tbl(KELuser, "tree") %>%
    filter(plot_id %in% old.plot.id) %>%
    select(treeid, status_old = status) %>%
    collect() %>%
    inner_join(., data$tree %>% select(treeid, species, status_new = status), by = "treeid") %>%
    filter(status_old %in% c(1:4) & !status_new %in% c(1:4)) %>%
    left_join(., data$mortality, by = "treeid") %>%
    mutate(date = ifelse(is.na(date), unique(data$plot$date), date),
           mort_agent = ifelse(is.na(mort_agent), 99, mort_agent),
           mort_agent = case_when(
             mort_agent %in% 99 & status_new %in% c(21:23) & species %in% "Picea abies" ~ 411,
             mort_agent %in% 99 & status_new %in% 0 ~ 71,
             mort_agent %in% 99 & status_new %in% 15 ~ 21,
             .default = mort_agent)) %>%
    distinct(., date, treeid, mort_agent)
  
  # tree
    
  data.clean$tree <- tbl(KELuser, "tree") %>%
    filter(plot_id %in% old.plot.id) %>%
    select(treeid, old_treen = treen) %>%
    collect() %>%
    right_join(., data$tree, by = "treeid") %>%
    inner_join(., data.clean$plot %>% select(plotid, plotsize, foresttype), by = "plotid") %>%
    left_join(., tbl(KELuser, "plot") %>%
                 filter(id %in% old.plot.id) %>%
                 select(plotid, plotsize_old = plotsize, dbh_min_old = dbh_min) %>%
                 collect(),
               by = "plotid") %>%
    mutate(distance_m = sqrt(abs(x_m^2) + abs(y_m^2)),
           onplot = case_when(
             is.na(distance_m) ~ 99,
             plotsize %in% 500 & distance_m <= 12.62 ~ 1,
             plotsize %in% 1000 & foresttype %in% c("spruce", "managed") & distance_m <= 17.84 ~ 1,
             plotsize %in% 1000 & foresttype %in% "beech" & distance_m <= 7.99 ~ 1,
             plotsize %in% 1000 & foresttype %in% "beech" & distance_m > 7.99 & distance_m <= 17.84 ~ 2,
             plotsize %in% 1500 & distance_m <= 7.99 ~ 1,
             plotsize %in% 1500 & distance_m > 7.99 & distance_m <= 17.84 ~ 2,
             plotsize %in% 1500 & distance_m > 17.84 & distance_m <= 21.85 ~ 3,
             .default = 0),
           census = case_when(
             !treetype %in% "0" ~ 0,
             plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 1] ~ 0,
             plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(5,6,7)] ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & is.na(distance_m) ~ 99,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 500 & distance_m > 12.62 ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 1000 & distance_m > 17.84 ~ 3,
             is.na(old_treen) & is.na(dbh_mm) ~ 99,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 500 & distance_m <= 12.62 & dbh_mm < dbh_min_old ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 1000 & distance_m <= 17.84 & dbh_mm < dbh_min_old ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 500 & distance_m <= 12.62 & dbh_mm >= dbh_min_old & dbh_mm <= dbh_min_old + 50 ~ 1,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 1000 & distance_m <= 17.84 & dbh_mm >= dbh_min_old & dbh_mm <= dbh_min_old + 50 ~ 1,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 500 & distance_m <= 12.62 & dbh_mm > dbh_min_old + 50 ~ 2,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & plotsize_old %in% 1000 & distance_m <= 17.84 & dbh_mm > dbh_min_old + 50 ~ 2,
             is.na(old_treen) & !plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & dbh_mm < dbh_min_old ~ 3,
             is.na(old_treen) & !plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & dbh_mm >= dbh_min_old & dbh_mm <= dbh_min_old + 50 ~ 1,
             is.na(old_treen) & !plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 3] & dbh_mm > dbh_min_old + 50 ~ 2,
             .default = 0)) %>%
    select(-old_treen, -plotsize, -foresttype, -plotsize_old, -dbh_min_old, -distance_m)

  # microsites
  
  data.clean$microsites <- data$microsites %>% 
    mutate(count = ifelse(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47), NA, count),
           method = 2)
 
  # deadwood
  
  data.clean$deadwood <- data$deadwood
  
  # regeneration
  
  data.clean$regeneration <- data$regeneration
  
  # regeneration_subplot
  
  data.clean$regeneration_subplot <- data$regeneration_subplot
  
  # soil
  
  # data.clean$soil <- data$soil
  
  # vegetation
  
  # data.clean$vegetation <- data$vegetation %>% mutate(gap_distance_m = NA)
  
  # habitat
  
  # data.clean$habitat <- data$habitat
  
  return(data.clean)
}
