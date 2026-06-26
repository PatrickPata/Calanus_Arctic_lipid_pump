# Organizing the Hydrobios multinet data
# March 20, 2026
# P. Pata
#
# The data file is an .xlsx spreadsheet with tabs named "zooplankton abundances" and "sampling details". The stratified nets typically have 0-20, 20-60, 60-100, 100-150, 150-250, then the rest depends on bottom depth.


library(tidyverse)
`%notin%` <- Negate(`%in%`)

fname <- "data/2023-2024-taxo_analyses_data_patrickPata-20260203.xlsx"

# ---- Load the hydrobios net data ----
hydrobios <- readxl::read_xlsx(
  fname,
  sheet = "zooplankton abundances",
  # To resolve expected numeric [the sampling net ID is loaded as numeric]
  col_types = c("text","text","text","text","text","numeric","text","numeric",
                "numeric","numeric","text","text","text","text","text","text",
                "text","numeric")) %>% 
  rename(abundance_m3 = `Total abundance (ind./m3 depth computed vol)`,
         volume_filtered_m3 = depth_calc_net_filtered_vol)

hydrobios.meta <- readxl::read_xlsx(fname,
                                    sheet = "sampling details")


# --- Match the Hydrobios and net data ----
# use the station_year instead of sample_id
load("data/uvp6_CNN_segmentations_morphotraits_20260320.RData")
rm(all.copepods.uvp6, region_rename, segmented.images, volumes.df)
uvp6_profiles <- Amund_UVP6_meta %>% 
  arrange(-bottom_depth_m) %>% 
  distinct(station_year, .keep_all = T) %>% 
  select(region.inuktitut, reg.inuk, station_year, 
         cruise_year = Year, uvp_leg = sample_cruise,
         STATION_NAME = sample_stationid_ed, 
         uvp_stn = sample_stationid, uvp_Date = object_date,
         uvp_Longtiude = object_lon, uvp_Latitude = object_lat) %>% 
  # manually edit the UVP station name to match net names
  mutate(STATION_NAME = if_else(STATION_NAME == "TCA_BB2",
                                "TCA-BB2", STATION_NAME),
         STATION_NAME = if_else(STATION_NAME == "CA4-23",
                                "C4-23", STATION_NAME)) 

# List of hydrobios stations 
hydrobios.meta2 <- hydrobios.meta %>% 
  group_by(STATION_NAME, 
           DEPLOYMENT_DATE_START, DEPLOYMENT_TIME_START, 
           BOTTOM_DEPTH, LATITUDE, LONGITUDE) %>% 
  summarise(minDepth = min(MIN_SAMPLE_DEPTH),
            maxDepth = max(MAX_SAMPLE_DEPTH),
            n_nets = length(as.numeric(unlist(strsplit(SAMPLING_NET_ID, "\\+")))), 
            .groups = "drop")  %>% 
  mutate(cruise_year = year(DEPLOYMENT_DATE_START)) 

# Match stations
# Note that some will be excluded later if the UVP6 profile will not be used
uvp_net_match <- left_join(hydrobios.meta2,
                           uvp6_profiles,
                           by = join_by(cruise_year, STATION_NAME)) %>% 
  filter(!is.na(uvp_stn)) 


# subset the hydrobios metadata to only include samples in uvp_net_match
hydrobios.meta <- hydrobios.meta %>% 
  filter(STATION_NAME %in% uvp_net_match$STATION_NAME)

# ------ Extract Calanus abundances -------
calanus <- hydrobios  %>% 
  # Select only stations in the UVP match dataset
  filter(STATION_NAME %in% uvp_net_match$STATION_NAME) %>% 
  mutate(YEAR = substr(DEPLOYMENT_DATE_START,1,4)) %>% 
  mutate(YEAR_STATION = paste(YEAR, STATION_NAME)) %>% 
  # Curate the data
  # For RA62: retain only 6+7+8+9 to get 2-100m abundance, exclude 6 & 7
  filter(!(YEAR_STATION == "2024 RA62" & SAMPLING_NET_ID %in% c("6","7"))) %>% 
  # Calculate depth  range and midpoint
  mutate(Depth_range = MAX_SAMPLE_DEPTH - MIN_SAMPLE_DEPTH) %>% 
  mutate(Depth_mid = (MAX_SAMPLE_DEPTH + MIN_SAMPLE_DEPTH) / 2) %>% 
  # Calculate /m2 abundane
  mutate(abundance_m2 = abundance_m3 * Depth_range) %>% 
  # Back calculate the number of individuals
  mutate(indiv_count = abundance_m3 * volume_filtered_m3) %>% 
  # Select only Calanus spp
  filter(grepl("Calanus", TAXON_ID) == T) %>% 
  # Differentiate C4-C6
  # mutate(stage = TAXON_LIFE_DEV_STAGE)
  mutate(stage = if_else(TAXON_LIFE_DEV_STAGE %in% c("C4","C5","M","F"),
                         TAXON_LIFE_DEV_STAGE, "N-C3"),
         stage = factor(stage, levels = c("C4","C5","M","F","N-C3"))) %>% 
  # Exclude nauplii to C3
  filter(stage != "N-C3") %>% 
  # remove 3 records of Calanus spp
  filter(TAXON_ID != "Calanus spp.") %>% 
  # remove Calanus finmarchicus C4 because size beyond range in images
  filter(!(TAXON_ID == "Calanus finmarchicus" &  
             TAXON_LIFE_DEV_STAGE == "C4"))


# ------- Calculate net integrated abundance and biomass ------
# Total of all Calanus spp
net.integ <- calanus %>%
  # filter(TAXON_ID != "Calanus finmarchicus") %>% 
  group_by(YEAR_STATION, STATION_NAME, DEPLOYMENT_DATE_START) %>% 
  summarise(total_abundance_m2 = sum(abundance_m2),
            mean_abundance_m3 = weighted.mean(abundance_m3, w = Depth_range),
            total_indiv_count = sum(indiv_count),
            maxDepth = max(MAX_SAMPLE_DEPTH),
            nbins = n(),
            .groups = "drop")

# ---- Export ----
save(uvp_net_match, hydrobios.meta, calanus, net.integ, 
     file = "data/hydrobios_net_calanus_20260321.RData")