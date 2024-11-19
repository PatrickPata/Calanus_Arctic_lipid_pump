# Explore Zooplankton community data of the Arctic
library(tidyverse)
library(gganimate)
library(ggdist)
library(lubridate)

# DFO Occurrence data 2006-2009, all vertical bongo net hauls
dfo <- read.csv(file = "C:/Users/patri/OneDrive - Université Laval/Laval_Postdoc/Arctic data/DFO 2006 2009 Zooplankton/occurrence.csv") %>% 
  select(sex:scientificNameAuthorship) %>% 
  select(-c(coordinatePrecision, specificEpithet, infraspecificEpithet,
            locationAccordingTo, samplingProtocol))

length(unique(dfo$scientificName))
length(unique(dfo$fieldNumber))

# Sp richness per sample
sp.rich <- dfo %>% 
  distinct(fieldNumber, year, month, day, eventTime, locality,
           minimumDepthInMeters, maximumDepthInMeters, 
           decimalLatitude, decimalLongitude,
           kingdom, phylum, class, order, family, genus, scientificName) %>% 
  group_by(fieldNumber, year, month, day, eventTime, locality,
           minimumDepthInMeters, maximumDepthInMeters, 
           decimalLatitude, decimalLongitude) %>% 
  summarise(nspecies = n(), .groups = "drop") %>% 
  mutate(year.day = year + (month/12) + (day/365))


# TODO Plot species richness through time and space
ggplot(sp.rich, aes(x = nspecies)) +
  geom_histogram()

ggplot(sp.rich, aes(x = year.day, y = nspecies)) +
  geom_point()

ggplot(sp.rich, aes(x = as.factor(year), y = nspecies)) +
  geom_boxplot()


# DFO Data of zooplankton biomass as major groups (~500 samples)
dfo.mg <- read.csv(file = "C:/Sync/Data/DFO/IOS_zooplankton_1980_2016_as_biomass_major_taxa_groups_EN.csv") %>% 
# Need to parse date, calculate total biomass

  mutate(date = lubridate::dmy(date)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         doy = yday(date)) %>% 
  filter(latitude > 60) %>%
  mutate(longitude = if_else(longitude > 0,
                             -180+(longitude*-1),
                             longitude)) %>% 
  rownames_to_column("Sample")

# # Get total biomass
dfo.mg$totalBiomass <- rowSums(dfo.mg[,15:49])
  
# unique(dfo.mg$region_name)

library(rnaturalearth)
library(marmap)
library(mapdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
basemap <- ggplot(data = world) +
  geom_sf(fill = "#A9A9A9") + 
  coord_sf(xlim = c(-170, -50), ylim = c(60, 85), expand = TRUE) +
  xlab("") + ylab("") + theme_bw() + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(0.8, "inch"),
        # legend.key.height = unit(0.5, "inch"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12)) 
# basemap


basemap +
# ggplot(dfo.mg, aes(x = longitude, y = latitude)) +
  geom_point(data = dfo.mg, 
             aes(x = longitude, y = latitude, color = totalBiomass),
             alpha = 0.5, size = 2) +
  scale_color_viridis_c(trans = "log10")


ggplot() +
geom_point(data = dfo.mg, 
           aes(x = longitude, y = latitude, color = totalBiomass),
           alpha = 0.5, size = 2) +
  scale_color_viridis_c(trans = "log10")


ggplot(dfo.mg,
       aes(x = (year + (doy/365)), y = latitude)) +
  geom_point(aes(color = longitude),
             alpha = 0.5, size = 3) +
  scale_color_viridis_c()


dfo.mg.long <- dfo.mg %>% 
  pivot_longer(cols = Polychaeta:Animalia,
               names_to = "majorgroup", values_to = "biomass") %>% 
  group_by(Sample) %>% 
  mutate(totalBiomass = sum(biomass)) %>% 
  ungroup() %>% 
  mutate(percBiomass = biomass / totalBiomass * 100)
  


ggplot(dfo.mg.long %>% 
         filter(majorgroup == "Copepoda"), 
       aes(x = longitude, y = latitude)) +
  geom_point(aes(color = percBiomass), 
             alpha = 0.5, size = 3) +
  scale_color_viridis_c()

ggplot(dfo.mg.long %>% 
         filter(majorgroup == "Copepoda"),
       aes(x = (year + (doy/365)), y = percBiomass)) +
  geom_point(aes(color = latitude),
             alpha = 0.5, size = 3) +
  scale_color_viridis_c()

ggplot(dfo.mg.long %>% 
         filter(majorgroup == "Chaetognatha") %>% 
         filter(percBiomass > 0), 
       aes(x = longitude, y = latitude)) +
  geom_point(aes(color = percBiomass), 
             alpha = 0.5, size = 3) +
  scale_color_viridis_c()
