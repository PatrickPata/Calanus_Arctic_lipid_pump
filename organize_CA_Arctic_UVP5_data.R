# UVP5 Data extraction for Canadian Arctic Area
# November 4, 2025
# P. Pata
#
# The data is sourced from the Kiko et al. (2022) UVP5 compilation of 2008-2022 observations

library(tidyverse)
fol <- 'C:/Sync/UVP5_Data_Kikoetal2022/'

# Column names
UVP5_chead <- read.csv(paste0(fol,'column_headers.csv'))



# Read each file then extract the Canadian Arctic region
# Lon lim: -90,-50
# Lat lim:  50, 90
# Note: no data in files 2008-2010, 2020-2022

# Part 1
df <- read.table(paste0(fol,'Ecopart_data_','2011_to_2013','.tsv'),
                 header = FALSE, sep = "\t", skip = 1)
colnames(df) <- UVP5_chead$AnnotColName

UVP5_CA_Arctic <- df %>% 
  filter(Longitude >= -90 & Longitude <= -50 &
           Latitude >= 50 & Latitude <= 90)
# Part 2
df <- read.table(paste0(fol,'Ecopart_data_','2014_to_2016','.tsv'),
                 header = FALSE, sep = "\t", skip = 1)
colnames(df) <- UVP5_chead$AnnotColName

UVP5_CA_Arctic <- UVP5_CA_Arctic %>% 
  bind_rows(df %>% 
              filter(Longitude >= -90 & Longitude <= -50 &
                       Latitude >= 50 & Latitude <= 90))

# Part 3
df <- read.table(paste0(fol,'Ecopart_data_','2017_to_2019','.tsv'),
                 header = FALSE, sep = "\t", skip = 1)
colnames(df) <- UVP5_chead$AnnotColName

UVP5_CA_Arctic <- UVP5_CA_Arctic %>% 
  bind_rows(df %>% 
              filter(Longitude >= -90 & Longitude <= -50 &
                       Latitude >= 50 & Latitude <= 90))

UVP5_CA_Arctic_meta <- UVP5_CA_Arctic %>% 
  group_by(Cruise, Project_id, Profile, RAWfilename, CTD_filename,
           Latitude, Longitude, Date_Time, Sample_id) %>% 
  summarise(Deepest_Bin = max(Pressure_dbar),
            Total_Volume = sum(Volume_L),
            .groups = "drop")


# Export file
save(file = "C:/Sync/UVP5_Data_Kikoetal2022/Canadian_Arctic_subset.RData",
     UVP5_CA_Arctic, UVP5_CA_Arctic_meta, UVP5_chead)

# Check map of distributions

# library(sf)
# library(rnaturalearth)
# world <- ne_countries(scale = "medium", returnclass = "sf")
# ggplot() +
#   geom_sf(data = world) +
#   theme_minimal() +
#   geom_point(data = df %>% 
#                distinct(Profile, .keep_all = TRUE), 
#              aes(x = Longitude, y = Latitude))
