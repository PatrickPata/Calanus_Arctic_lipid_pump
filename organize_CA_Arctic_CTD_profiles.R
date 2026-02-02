# Organize CTD profiles linked to UVP6 casts
# P. Pata
# November 24, 2025
#
# Data downloaded from: https://catalogue.amundsenscience.com/en/dataset/ca-cioos_ccin-12713
# Forest, A., Guillot, P., Desmarais, A., Lévesque, K., Linkowski, T., Morisset, S., Wilhelmy, C., & Zier, J. (2025). CTD data collected by the CCGS Amundsen in the Canadian Arctic [Data set]. https://doi.org/10.5884/12713
#
# Data is stored in 3 .csv files for the respective 2021, 2023, and 2024 periods
# Additional data for 2024 Leg 5a

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# Get the list of excluded UVP6 profiles
load("data/UVP6_Calanus_metadata_20260129.RData")
rm(volumes.df, region_rename, pg_meta)

fol <- "C:/Sync/UVP6_Data/CTD_profiles/"
file <- paste0(fol, "amundsen12713_317e_649b_ef03.csv")
file_2024_leg5a <- paste0(fol,"CTD_Amundsen_2024_Leg5/classic/data_CTD_leg5a.csv")


ctd <- read.csv(file) %>% 
  filter(cruise_name %in% c("2105_AS",
                            "2023_02_AS","2023_03_AS",
                            "2024_02_AS","2024_03_AS","2024_04_AS"))

# Fix column names
cnames <- read.csv(paste0(fol, "ctd_column_names.csv"))
colnames(ctd) <- cnames$annotated_name

# add the 2024 leg 5a data
ctd.5a <- read.csv(file_2024_leg5a) %>% 
  select(-X) 
cnames <- read.csv(paste0(fol, "ctd_column_names_2024_leg5a.csv"))
colnames(ctd.5a) <- cnames$annotated_name

# Add additional info to 2024 leg5a data. 
# Note that the UVP 2024-leg 5 metadata does not have the CTD filenames or cast number so matching these by date and station name instead
ctd.5a <- ctd.5a %>% 
  mutate(cruise_name = "2024_05_AS",
         platform_name = "Amundsen", platform_id = 7510846) %>% 
  # remove columns not in the main ctd file
  select(any_of(colnames(ctd)))


# TODO Append the leg 5a data

ctd <- ctd %>% 
  bind_rows(ctd.5a) %>% 
  arrange(datetime)

ctd_profiles <- ctd %>% 
  group_by(cruise_name, station, filename) %>% 
  mutate(max_pres = max(pressure),
         max_dep = max(depth),
         min_pres = min(pressure),
         min_dep = min(depth)) %>% 
  ungroup() %>% 
  # distinct(cruise_name, station, .keep_all = T) %>% 
  distinct(cruise_name, station, filename, .keep_all = T) %>%
  select(platform_name:longitude, max_pres:min_dep) 

# ** Match CTD and UVP6 casts **

# Open metadata for UVP6 casts (the xlsx file has more metadata)
uvp6_profiles <- readxl::read_xlsx("C:/Sync/UVP6_Data/UVP6_Data_Overview.xlsx",
                                   sheet = "ECOPART_summary") %>% 
  filter(Profile %in% Amund_UVP6_meta$sample_id) %>% 
  filter(Profile %notin% to_exclude$sample_id) %>% 
  # The UVP6 stations named MAK-2024-M1-Far and MAK-2024-M1-Near are called, MAK-2024-M1 only. Adjust the station names
  mutate(station = if_else(Profile == "am_leg2_makM1far_1", "MAK-2024-M1", station),
         station = if_else(Profile == "am_leg2_makM1near_1", "MAK-2024-M1", station)) %>% 
  # To fix the darkedge ctd matching 
  # The cast numbers for de440_004_yo# are all 34 & filenames 2105034, which do not exist. Assuming the sequence is correct because the times are close by, adjust to 36-38.
  # Similarly for de430_004_yo#, cast number 39 does not exist. The casts were made on 2021-10-23 3-4 hour and this only matches with the CTD cast 34 at 2021-10-23T03:25:34Z.
  mutate(cast_number = if_else(Profile == "de440_004_yo01", 36, cast_number),
         cast_number = if_else(Profile == "de440_004_yo02", 37, cast_number),
         cast_number = if_else(Profile == "de440_004_yo03", 38, cast_number),
         cast_number = if_else(Profile %in% c("de430_004_yo01","de430_004_yo02","de430_004_yo03"),
                               34, cast_number)) %>% 
  # for the am_leg 2 data, fix wrong cast numbers
  mutate(cast_number = if_else(Profile == "am_leg2_tcaqf4", 35, cast_number),
         cast_number = if_else(Profile == "am_leg2_c5", 43, cast_number)) %>% 
  # for am_leg 5, the ctd data does not have cast 77 for TCAT6-03(A3) am_leg5_TCAT6_03_03, so use cast 75 instead
  mutate(cast_number = if_else(Profile == "am_leg5_TCAT6_03_03", 75, cast_number))

# Matching based on Cruise-Station-Cast number
uvp6_ctd_match <- left_join(uvp6_profiles, ctd_profiles,
                            join_by(cruise_name, station, cast_number)) %>% 
  relocate(Cruise, Profile, Site, cast_number, 
           CTDrosettefilename, filename,
           `yyyy-mm-dd hh:mm`, datetime) %>% 
  select(platform_name, Cruise, Site, ctd_station = station, cast_number,
         uvp6_date = `yyyy-mm-dd hh:mm`,
         uvp6_longitude = Longitude, uvp6_latitude = Latitude,
         ctd_filename = filename,
         ctd_date = datetime,
         ctd_longitude = longitude, ctd_latitude = latitude,
         ctd_maxpres = max_pres, ctd_maxdep = max_dep,
         Rawfilename, DataOwner, 
         profile_group, region)

# Which UVP6 profiles have no associate CTD data
no.ctd <- uvp6_ctd_match %>% 
  filter(is.na(ctd_filename))


# ** Filter the ctd profiles to only include the casts associated with UVP6 data
ctd_profiles <- ctd_profiles %>%
    filter(filename %in% uvp6_ctd_match$ctd_filename)
ctd <- ctd %>% 
  filter(filename %in% uvp6_ctd_match$ctd_filename) %>% 
  # ph and turbidity are always NA so remove
  select(-c(ph, turb))

ctd_cnames <- cnames

save(file = "data/uvp6_associated_ctd_20260202.RData",
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

AA <- ctd %>% filter(cruise_number == "2024_05")
AA.ctd <- oce::as.ctd(salinity = AA$psal,
                      temperature = AA$te90,
                      pressure = AA$pressure,
                      time = AA$datetime,
                      ship = AA$platform_name,
                      cruise = AA$cruise_name,
                      station = AA$station,
                      longitude = AA$longitude,
                      latitude = AA$latitude)

oce::plotTS(AA.ctd)
