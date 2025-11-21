# Organize CA Arctic UVP6 Particle data
# November 20, 2025
# P. Pata
#
# This organizes the ECOPART aggregated detailed partile export data found in folder C:\Sync\UVP6_Data\Ecopart.
# In each project, a version of the particle data was exported for all_particles and living_particles only. This was done so that the "detritus" particles can be easily extracted by subtraction. Note that for the 2024 Amundsen data, the projects are not 100% validated so the assignment of living and nonliving are mostly based on the EcoTaxa classification algorithm.
#
# UVP6 detailed 45 Particles class limits (Upper limit included + 1 extra class > last limit): 1 µm, 1.26 µm, 1.59 µm, 2 µm, 2.52 µm, 3.17 µm, 4 µm, 5.04 µm, 6.35 µm, 8 µm, 10.1 µm, 12.7 µm, 16 µm, 20.2 µm, 25.4 µm, 32 µm, 40.3 µm, 50.8 µm, 64 µm, 80.6 µm, 102 µm, 128 µm, 161 µm, 203 µm, 256 µm, 323 µm, 406 µm, 512 µm, 645 µm, 813 µm, 1.02 mm, 1.29 mm, 1.63 mm, 2.05 mm, 2.58 mm, 3.25 mm, 4.1 mm, 5.16 mm, 6.5 mm, 8.19 mm, 10.3 mm, 13 mm, 16.4 mm, 20.6 mm, 26 mm
#
# Only Size classes 18 (50.8-64 um) to 45 (>26) will have data. Thus, size classes 1-17 are removed from the data tables.

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# Open file of column names
uvp6_par_cnames <- read.csv('data/Ecopart_UVP6_detailed_PAR_file_column_names.csv')

# # Note the the column headers need to be formatted properly


read_par_tsv <- function(filename, fol = "C:/Sync/UVP6_Data/Ecopart/") {
   df_ed <- read.table(paste0(fol, filename, ".tsv"),  
                       header = FALSE, skip = 1)
  colnames(df_ed) <- uvp6_par_cnames$AnnotatedColumnName

  # Will only have values starting size class 18 (50.8-64 um)
  df_ed <- df_ed %>%
    select(-c(concentration_sc_1:concentration_sc_17,
              biovolume_sc_1:biovolume_sc_17))
}


df1 <- read_par_tsv("2021_Leg5/all_particles/export_detailed_20251120_16_43_PAR_Aggregated")
df2 <- read_par_tsv("2023_Leg2/all_particles/export_detailed_20251120_16_46_PAR_Aggregated")
df3 <- read_par_tsv("2024_Leg4/all_particles/export_detailed_20251120_16_29_PAR_Aggregated") 
df4 <- read_par_tsv("2024_Leg235/all_particles/export_detailed_20251120_16_49_PAR_Aggregated") 

uvp6_PAR <- bind_rows(df1, df2, df3, df4)
rm(df1, df2, df3, df4)

uvp6_profiles <- readxl::read_xlsx("C:/Sync/UVP6_Data/UVP6_Data_Overview.xlsx",
                                   sheet = "ECOPART_summary")

save(file = "data/UVP6_ECOPART_Data_20251121.RData",
     uvp6_par_cnames, uvp6_PAR, uvp6_profiles)

# # No differences in exported particle contentrations for living and all!
# df1a <- read_par_tsv("2021_Leg5/all_particles/export_detailed_20251120_16_43_PAR_Aggregated")
# df1b <- read_par_tsv("2021_Leg5/living_particles/export_detailed_20251104_23_01_PAR_Aggregated")
# all.equal(df1a,df1b)
