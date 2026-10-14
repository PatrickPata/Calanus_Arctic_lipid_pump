# Organize the POC flux vs LPC flux comparison data
# 
# Modified June 26, 2026
# This replaces the 8-compare_gravity_lipid_pump.Rmd file

# Load the library and data
library(tidyverse)
`%notin%` <- Negate(`%in%`)
theme_set(theme_bw())

# Generated in script file 4-*.Rmd:
load("data/uvp6_images_calculated_traits_20260321.RData")
# Generated in script file 5-*.Rmd
load("data/profile_integrated_cflux_20260511.RData")
# Generated in script file 7-*.Rmd:
load("data/POC_flux_UVP6_20260515.RData")

# Prepare comparisons

# Pairwise matching of POC flux and lipid flux
lipid.Cflux.profile.200 <- df.profile %>% 
  filter(station_year %in% pocFlux.ave.200$station_year) %>% 
  filter(taxa == "Calanus") %>% 
  rename(lipid_gC_m2_yr = total_gC_flux_m2) %>% 
  select(reg.inuk, station_year, lipid_gC_m2_yr) %>% 
  # calculate daily flux
  mutate(lipid_mgC_m2_d = lipid_gC_m2_yr/365 * 1000) 

lipid.Cflux.region.200 <- lipid.Cflux.profile.200 %>% 
  group_by(reg.inuk) %>% 
  summarise(lipid_gC_m2_yr_mean = mean(lipid_gC_m2_yr, na.rm = T),
            lipid_gC_m2_yr_sd = sd(lipid_gC_m2_yr, na.rm = T),
            lipid_mgC_m2_d_mean = mean(lipid_mgC_m2_d, na.rm = T),
            lipid_mgC_m2_d_sd = sd(lipid_mgC_m2_d, na.rm = T),
            n = n(), .groups = "drop")

lipid.Cflux.profile.100 <- df.profile %>% 
  filter(station_year %in% pocFlux.ave.100$station_year) %>% 
  filter(taxa == "Calanus") %>% 
  rename(lipid_gC_m2_yr = total_gC_flux_m2) %>% 
  select(reg.inuk, station_year, lipid_gC_m2_yr) %>% 
  # calculate daily flux
  mutate(lipid_mgC_m2_d = lipid_gC_m2_yr/365 * 1000) 

lipid.Cflux.region.100 <- lipid.Cflux.profile.100 %>% 
  group_by(reg.inuk) %>% 
  summarise(lipid_gC_m2_yr_mean = mean(lipid_gC_m2_yr, na.rm = T),
            lipid_gC_m2_yr_sd = sd(lipid_gC_m2_yr, na.rm = T),
            lipid_mgC_m2_d_mean = mean(lipid_mgC_m2_d, na.rm = T),
            lipid_mgC_m2_d_sd = sd(lipid_mgC_m2_d, na.rm = T),
            n = n(), .groups = "drop")

lipid.Cflux.profile.500 <- df.profile %>% 
  filter(station_year %in% pocFlux.ave.500$station_year) %>% 
  filter(taxa == "Calanus") %>% 
  rename(lipid_gC_m2_yr = total_gC_flux_m2) %>% 
  select(reg.inuk, station_year, lipid_gC_m2_yr) %>% 
  # calculate daily flux
  mutate(lipid_mgC_m2_d = lipid_gC_m2_yr/365 * 1000) 

lipid.Cflux.region.500 <- lipid.Cflux.profile.500 %>% 
  group_by(reg.inuk) %>% 
  summarise(lipid_gC_m2_yr_mean = mean(lipid_gC_m2_yr, na.rm = T),
            lipid_gC_m2_yr_sd = sd(lipid_gC_m2_yr, na.rm = T),
            lipid_mgC_m2_d_mean = mean(lipid_mgC_m2_d, na.rm = T),
            lipid_mgC_m2_d_sd = sd(lipid_mgC_m2_d, na.rm = T),
            n = n(), .groups = "drop")

# Combine data frames for comparisons
flux.comp.100 <- left_join(lipid.Cflux.region.100, pocFlux.region.100,
                           by = join_by(reg.inuk, n))
flux.comp.200 <- left_join(lipid.Cflux.region.200, pocFlux.region.200,
                           by = join_by(reg.inuk, n))
flux.comp.500 <- left_join(lipid.Cflux.region.500, pocFlux.region.500,
                           by = join_by(reg.inuk, n))

flux.com.profiles.100 <- left_join(lipid.Cflux.profile.100,
                                   pocFlux.ave.100,
                                   by = join_by(reg.inuk, station_year))
flux.com.profiles.200 <- left_join(lipid.Cflux.profile.200,
                                   pocFlux.ave.200,
                                   by = join_by(reg.inuk, station_year))
flux.com.profiles.500 <- left_join(lipid.Cflux.profile.500,
                                   pocFlux.ave.500,
                                   by = join_by(reg.inuk, station_year))

# Add figure legend labels
flux.comp.100 <- flux.comp.100 %>%
  mutate(region_label = paste0(reg.inuk, "\n(n=", n,")"))
flux.comp.200 <- flux.comp.200 %>%
  mutate(region_label = paste0(reg.inuk, "\n(n=", n,")"))
flux.comp.500 <- flux.comp.500 %>%
  mutate(region_label = paste0(reg.inuk, "\n(n=", n,")"))

# Exclude regions with less than 3 profiles to allow proper comparison
flux.comp.500 <- flux.comp.500 %>% 
  filter(n > 3) %>% 
  droplevels()


# # Export comparison results
# save(flux.comp.100, flux.comp.200, flux.comp.500, 
#      flux.com.profiles.100, flux.com.profiles.200, flux.com.profiles.500,
#      file = "data/UVP6_POC_lipidC_flux_comparisons_20260515.RData")
