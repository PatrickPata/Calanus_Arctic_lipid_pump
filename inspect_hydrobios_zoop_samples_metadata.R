# Match UVP casts with Hydrobios stations file
# January 20, 2026
# P. Pata

library(tidyverse)

# List of UVP casts
uvp6_profiles <- readxl::read_xlsx("C:/Sync/UVP6_Data/UVP6_Data_Overview.xlsx",
                                   sheet = "ECOPART_summary") %>% 
  filter(!is.na(region)) %>% 
  # exclude darkedge
  filter(Cruise != "darkedge") %>% 
  # assign year
  mutate(cruise_year = year(`yyyy-mm-dd hh:mm`))


# List of hydrobios data
hydrobios <- readxl::read_xlsx("data/2023-2024-available_hydrobios_analyses-PPata-202512142356.xlsx")
hydrobios.meta <- hydrobios %>% 
  group_by(cruise_year, leg, STATION_NAME, 
           DEPLOYMENT_DATE_START, DEPLOYMENT_TIME_START, 
           BOTTOM_DEPTH, LATITUDE, LONGITUDE) %>% 
  summarise(minDepth = min(MIN_SAMPLE_DEPTH),
            maxDepth = max(MAX_SAMPLE_DEPTH),
            nbins = n(), .groups = "drop") 



# TODO Check if stations match
uvp6.meta <- uvp6_profiles %>% 
  select(cruise_year, uvp_leg = Cruise, Profile,
         STATION_NAME = Site, uvp_stn = station, region,
         uvp_Date = `yyyy-mm-dd hh:mm`,
         uvp_Longtiude = Longitude,
         uvp_Latitude = Latitude)
uvp_net_match <- left_join(hydrobios.meta,
                           uvp6.meta %>% 
                             # manually edit the UVP station name to match here
                             mutate(STATION_NAME = if_else(STATION_NAME == "TCA_BB2",
                                                           "TCA-BB2", STATION_NAME),
                                    STATION_NAME = if_else(STATION_NAME == "CA4-23",
                                                           "C4-23", STATION_NAME)),
                           by = join_by(cruise_year,
                                        STATION_NAME)) 

# These sites do not have matching station names:
#   2023: A7/196, Lsea-23, NB-02
#   2024: TCAT6-01(A1), TCAT6-07(A7)


# Do they match in lon & lat?
cowplot::plot_grid(
  ggplot(uvp_net_match, aes(x = LONGITUDE, y = uvp_Longtiude)) +
    geom_point() +
    geom_abline(),
  ggplot(uvp_net_match, aes(x = LATITUDE, y = uvp_Latitude)) +
    geom_point() +
    geom_abline()
)

# # Stations with repeated uvp casts?
# view( uvp_net_match %>% 
#   group_by(cruise_year, STATION_NAME) %>% 
#     filter(n() > 1))



# Filter to stations with multiple tows and maxDepth >= 250?
hydrobios_to_get <- uvp_net_match %>%
  distinct(cruise_year, STATION_NAME, .keep_all = T) %>% 
  filter(nbins > 4) %>% 
  filter(maxDepth >= 250) %>% 
  # only with matching UVP cast
  filter(!is.na(uvp_stn))


# Show in map
library(ggOceanMaps)
`%notin%` <- Negate(`%in%`)

basemap("ArcticStereographic", bathymetry = FALSE, glaciers = FALSE,
        limits = c(-85,-52,65,83), rotate = T,
        legends = TRUE, land.col = "gray60",  bathy.style = "contour_grays",
        land.border.col = "black", gla.border.col = NA) +
  ggspatial::geom_spatial_point(data = hydrobios_to_get,
                                aes(x = LONGITUDE, y = LATITUDE,
                                    shape = uvp_leg,
                                    color = BOTTOM_DEPTH),
                                size = 3, alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", direction = -1,
                       name = "Bottom depth (m)") +
  theme(axis.title = element_blank(),
        legend.position = "right",
        legend.box.background = element_rect(fill = "white", color = "black")) +
  # Add not included stations
  ggspatial::geom_spatial_point(data = hydrobios.meta %>% 
                                  filter(STATION_NAME %notin% hydrobios_to_get$STATION_NAME),
                                aes(x = LONGITUDE, y = LATITUDE), 
                                color = "red", shape = 8,
                                size = 2, alpha = 0.8) 

ggsave("figures/hydrobios_stations.png",
       width = 7, height = 7, units = "in", bg = "white")
