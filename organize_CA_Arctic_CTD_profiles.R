# Organize CTD profiles linked to UVP6 casts
# P. Pata
# November 24, 2025
#
# Data downloaded from: https://catalogue.amundsenscience.com/en/dataset/ca-cioos_ccin-12713
# Forest, A., Guillot, P., Desmarais, A., Lévesque, K., Linkowski, T., Morisset, S., Wilhelmy, C., & Zier, J. (2025). CTD data collected by the CCGS Amundsen in the Canadian Arctic [Data set]. https://doi.org/10.5884/12713
#
# Data is stored in 3 .csv files for the respective 2021, 2023, and 2024 periods


library(tidyverse)
`%notin%` <- Negate(`%in%`)

fol <- "C:/Sync/UVP6_Data/CTD_profiles/"
file <- paste0(fol, "amundsen12713_317e_649b_ef03.csv")


ctd <- read.csv(file) %>% 
  filter(cruise_name %in% c("2105_AS",
                            "2023_02_AS","2023_03_AS",
                            "2024_02_AS","2024_03_AS","2024_04_AS"))

# TODO fix column names
cnames <- read.csv(paste0(fol, "ctd_column_names.csv"))
colnames(ctd) <- cnames$annotated_name


ctd <- ctd %>% 
  arrange(datetime)

ctd_profiles <- ctd %>% 
  group_by(cruise_name, station) %>% 
  mutate(max_pres = max(pressure),
         max_dep = max(depth)) %>% 
  distinct(cruise_name, station, .keep_all = T) %>% 
  select(platform_name:longitude, max_pres, max_dep) 


# ** Match CTD and UVP6 casts **

# Open metadata for UVP6 casts
uvp6_profiles <- readxl::read_xlsx("C:/Sync/UVP6_Data/UVP6_Data_Overview.xlsx",
                                   sheet = "ECOPART_summary")

# # Assign by cast number? - Same station multiple casts do not have associated CTD
# AB <- left_join(uvp6_profiles, ctd_profiles,
#                 join_by(cruise_name, cast_number)) %>% 
#                 # join_by(Site == station))
#   relocate(platform_name, station.x, station.y)

# Assign by station name?
uvp6_ctd_match <- left_join(uvp6_profiles, ctd_profiles,
                join_by(cruise_name, station)) %>% 
  select(platform_name, Cruise, Site, ctd_station = station, 
         uvp6_cast_number = cast_number.x, ctd_cast_number = cast_number.y,
         uvp6_date = `yyyy-mm-dd hh:mm`,
         uvp6_longitude = Longitude, uvp6_latitude = Latitude,
         ctd_filename = filename,
         ctd_date = datetime,
         ctd_longitude = longitude, ctd_latitude = latitude,
         ctd_maxpres = max_pres, ctd_maxdep = max_dep,
         Rawfilename, DataOwner)

# Z <- filter(uvp6_ctd_match, is.na(platform_name)) %>%
#   filter(Cruise != "am_leg5")
# 
# # unique ctd casts with UVP6 data
# length(unique(uvp6_ctd_match$ctd_filename))
# 
# AA <- ctd_profiles %>%
#   filter(filename %notin% uvp6_ctd_match$ctd_filename)


# ** Filter the ctd profiles to only include the casts associated with UVP6 data
ctd_profiles <- ctd_profiles %>%
    filter(filename %in% uvp6_ctd_match$ctd_filename)
ctd <- ctd %>% 
  filter(filename %in% uvp6_ctd_match$ctd_filename) %>% 
  # ph and turbidity are always NA so remove
  select(-c(ph, turb))

ctd_cnames <- cnames

save(file = "data/uvp6_associated_ctd_20251124.RData",
     ctd, ctd_profiles, uvp6_ctd_match, ctd_cnames)


# ggplot(ctd %>% 
#          filter(!is.na(te90)), 
#        aes(x = depth, y = te90, color = psal) ) +
#   geom_point() +
#   coord_flip() +
#   scale_x_reverse() +
#   scale_color_viridis_c() +
#   facet_wrap(~cruise_name, scales = "free")
# 
# 
# ggplot(ctd %>% 
#          filter(!is.na(psal)), 
#        aes(x = depth, y = psal, color = te90) ) +
#   geom_point() +
#   coord_flip() +
#   scale_x_reverse() +
#   scale_color_viridis_c() +
#   facet_wrap(~cruise_name, scales = "free")

# AA <- ctd %>% filter(cruise_number == "2021005")
# AA.ctd <- oce::as.ctd(salinity = AA$psal,
#                       temperature = AA$te90,
#                       pressure = AA$pressure,
#                       time = AA$datetime,
#                       ship = AA$platform_name,
#                       cruise = AA$cruise_name,
#                       station = AA$station,
#                       longitude = AA$longitude,
#                       latitude = AA$latitude)  
#   
# oce::plotTS(AA.ctd)
