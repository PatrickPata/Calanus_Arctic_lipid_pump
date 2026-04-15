# Organizing the UVP6 data
# P. Pata
# March 19, 2026
#
# Organization of the imaging data. Additional information such as sampled volume, in situ temperature, and regional assignments are established.
#
# Whenever the CNN segmentation pipeline is updated, this file needs to be updated as well.
#
#

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# Folder containing the predicted images and summary table file
fol <- "C:/Sync/UVP6_Data/lipid_predictions_20251123/"

# Load and curate the image data
# 1. Load and select only the necessary columns
df.CNN.seg <- read.csv(paste0(fol, "new_predictions_uvp6_20251123.csv")) %>% 
  mutate(object_id = str_remove(filename, ".png")) %>% 
  select(object_id, 
         prosome_pixels = prosome_pixels_predicted,
         prosome_major = prosome_major_predicted,
         lipid_pixels = lipidpcrop_pixels_predicted,
         lipid_major = lipidpcrop_major_predicted) %>% 
  mutate(source = "CNN_segmentation")

# 2. Mark images found inside the bad images folder
# List file names of images inside the folder containing predicted images evaluated as bad segmentations
bad.images.files <- list.files("C:/Sync/UVP6_Data/lipid_predictions_20251123/bad_segmentations/")

df.CNN.seg <- df.CNN.seg %>% 
  mutate(is_bad_image = if_else(object_id %in% 
                                  str_remove(bad.images.files, "_overlapRGB.png"),
                                T,F)) %>% 
  relocate(object_id)


# 3. Switch in the manually segmented images. Only the images that have both prosome and lipid annotations.
# The output files of the model training
res.prosome <-  read.csv("C:/Users/patri/python_workspace/copepods-lipid-content/prediction_outputs/model_resnet34_prosomeUVP6_deamundp1_v2/prediction_prosome_uvp6_20251204.csv")

res.lipid.crop <-  read.csv("C:/Users/patri/python_workspace/copepods-lipid-content/prediction_outputs/model_resnet34_lipidcropUVP6_deamundp1_v2/prediction_lipids_uvp6_20251204.csv")

# Note that there are 2 images manually segmented that are not in the segmented images folder, because the taxa categorization changed 
df.manual <- inner_join(res.prosome, res.lipid.crop,
                       by = join_by(filename)) %>% 
  mutate(object_id = str_remove(filename, ".png")) %>% 
  select(object_id, 
         prosome_pixels = prosome_pixels_annotated,
         prosome_major = prosome_major_annotated,
         lipid_pixels = lipid_pixels_annotated,
         lipid_major = lipid_major_annotated) %>%  
  mutate(source = "manual_segmentation") %>% 
  mutate(is_bad_image = F)

df.CNN.seg <- df.CNN.seg %>% 
  filter(object_id %notin% df.manual$object_id) %>% 
  bind_rows(df.manual)

rm(bad.images.files, res.prosome, res.lipid.crop)

# 4. Add the morphological characteristics information

# Open morphological traits and metadata
load("C:/Sync/UVP6_Data/Amund_UVP6_plankton_20251125.RData")
# Join the datasets
segmented.images <- df.CNN.seg %>% 
  left_join(Amund_UVP6, by = "object_id") %>% 
  select(-c(twilight.x, twilight.y)) %>% 
  rename(
    Major = object_major, Minor = object_minor, 
    Perim = object_perim., Feret = object_feret, 
    Area = object_area, Elongation = object_elongation,
    VerSym = object_symetriev, VerSym2 = object_symetrievc, 
    Circ = object_circ., Thickr = object_thickr, 
    MeanGrey = object_mean, MedGrey = object_median,
    HistGrey75 = object_histcum3, SdevGrey = object_stddev, 
    SkewGrey = object_skew, Fractal = object_fractal, 
    PerimFeret = object_perimferet, PerimMajor = object_perimmajor
  )

# 4. Calculate the size ranges of the manually segmented images
manual.segmented.images <- segmented.images %>% 
  filter(source == "manual_segmentation") %>% 
  select(object_id, Major, prosome.major = prosome_major, 
         prosome.pixels = prosome_pixels, 
         lipid.pixels = lipid_pixels) %>% 
  # convert to mm
  mutate(Major.mm = Major * 0.073, 
         prosome.major.mm = prosome.major * 0.073)

sum.manual.seg <- manual.segmented.images %>% 
  select(-object_id) %>% 
  summarise(across(everything(), list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n = ~sum(!is.na(.)) # Count of non-NA values
  ), .names = "{.col}_{.fn}")) %>% 
  pivot_longer(cols = everything(), names_to = "statistic", values_to = "value") %>%
  separate(statistic, into = c("column", "metric"), sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value)


# 5. Prepare data frame of all the copepods and their morphotraits 
# Note: currently includes unverified taxonomy
all.copepods.uvp6 <- Amund_UVP6 %>% 
  filter(taxagroup %in% c("Copepoda", "Copepoda-lipidsac")) %>% 
  # remove images predicted as dead/appendages/body parts, cyclopoids, harpacticoids, and other definitely not Calanus spp calanoids
  filter(object_annotation_category %notin% 
           c("antenna<Calanoida","antenna<Copepoda","with-eggs<Calanoida",
             "copepoda eggs","dead<Copepoda","part<Copepoda",
             "Cyclopoida","Harpacticoida","Heterorhabdus","Heterorhabdidae",
             "Paraeuchaeta","Metridia","female+eggs<Paraeuchaeta")) %>% 
  select(-c(twilight.x, twilight.y)) %>% 
  rename(
    Major = object_major, Minor = object_minor, 
    Perim = object_perim., Feret = object_feret, 
    Area = object_area, Elongation = object_elongation,
    VerSym = object_symetriev, VerSym2 = object_symetrievc, 
    Circ = object_circ., Thickr = object_thickr, 
    MeanGrey = object_mean, MedGrey = object_median,
    HistGrey75 = object_histcum3, SdevGrey = object_stddev, 
    SkewGrey = object_skew, Fractal = object_fractal, 
    PerimFeret = object_perimferet, PerimMajor = object_perimmajor
  ) %>% 
  # Retain Major axis length to > 3mm (most likely to be Calanus spp) given that the 5% quantile is 2.81 mm and the mean-sd is 3.12 mm. Note that this is not the prosome length.
  mutate(major_length_mm = Major * 0.073) %>% 
  # Of the images not segmented, only retain major lengths > 3 mm
  filter(major_length_mm >= 3 | object_id %in% segmented.images$object_id)

# Also adjust the metadata columns
Amund_UVP6_meta <- Amund_UVP6_meta %>% 
  select(-c(twilight.x, twilight.y)) %>% 
  mutate(Month_Name = month(object_date, label = TRUE, abbr = FALSE))

rm(Amund_UVP6)

# 6. Mark and exclude the CNN segmentations that are beyond the range of sizes according to the manually segmented images
to_exclude_by_size <- segmented.images %>% 
  filter(source == "CNN_segmentation") %>% 
  # exclude all images smaller than the minimum size of the training data, also remove too small/large lipid and prosome pixel area!
  filter(prosome_major < filter(sum.manual.seg,
                                column == "prosome.major")$min |
           prosome_major > filter(sum.manual.seg, 
                                  column == "prosome.major")$max |
           prosome_pixels < filter(sum.manual.seg, 
                                   column == "prosome.pixels")$min |
           prosome_pixels > filter(sum.manual.seg, 
                                   column == "prosome.pixels")$max |
           
           lipid_pixels < filter(sum.manual.seg, 
                                 column == "lipid.pixels")$min |
           lipid_pixels > filter(sum.manual.seg, 
                                 column == "lipid.pixels")$max)

segmented.images <- segmented.images %>% 
  mutate(exclude_by_size = if_else(object_id %in% to_exclude_by_size$object_id, 
                                   T,F))

# *** Apply data curation ***
# only manually segmented, not bad images, and not excluded by size
# This will exclude 801 images (733 bad, 68 more with extrapolated sizes) out of 15244 images originally segmented (5.3%).
segmented.images <- segmented.images %>% 
  filter(source == "manual_segmentation" |
           (is_bad_image == F & exclude_by_size == F)) %>% 
  select(-c(exclude_by_size, is_bad_image))

# Mark which images in the morpho traits data frame could be used for the RF
all.copepods.uvp6 <- all.copepods.uvp6 %>% 
  mutate(traits_from_RF = if_else(object_id %in% segmented.images$object_id,
                                  F, T))

# 7 . Add bottom depth information 

# Open the table of Amundsen sampling stations which has bottom depth info
stns_meta <- read.csv("data/Amundsen_stations_list_ed20251201.csv") %>% 
  select(sample_cruise = Cruise,  
         sample_stationid = station_id,
         Amund_Station_ID = Station.ID,
         bottom_depth_m = `Depth..m.`) %>% 
  filter(sample_stationid != "") %>% 
  # Choose max depth
  group_by(sample_cruise, sample_stationid) %>% 
  mutate(bottom_depth_m = max(bottom_depth_m)) %>% 
  ungroup() %>% 
  distinct(sample_cruise, sample_stationid, .keep_all = T)


# Add bottom depth to Amund_UVP metadata
Amund_UVP6_meta <- Amund_UVP6_meta %>% 
  select(-object_id) %>% 
  # For 2021 darkedge, the station id is only the first segment
  mutate(sample_stationid_ed = if_else(sample_cruise == "darkedge",
                                       str_extract(sample_id, "^[^_]+"), 
                                       sample_stationid)) %>% 
  mutate(sample_stationid_ed = if_else(sample_id == "de_bb2_float",
                                       "de_bb2_float", sample_stationid_ed)) %>% 
  # modify sample_stationid_ed for MAK-2024-M1
  mutate(sample_stationid_ed = if_else(
    sample_stationid_ed %in% c("MAK-2024-M1-Near","MAK-2024-M1-Far"),
    "MAK-2024-M1", sample_stationid_ed)) %>% 
  
  left_join(stns_meta, 
            by = join_by(sample_cruise, 
                         sample_stationid_ed == sample_stationid)) %>% 
  # For profiles with no assigned bottom depth, use the max sampling depth
  mutate(bottom_depth_m = if_else(is.na(bottom_depth_m), 
                                  maxSampleDepth, bottom_depth_m)) %>%
  # Correct the bottom depth for am_leg5_TCA_T3_03_01
  mutate(bottom_depth_m = if_else(sample_id == "am_leg5_TCA_T3_03_01", 
                                  maxSampleDepth, bottom_depth_m)) %>%
  # Cast ratio
  mutate(castratio = maxSampleDepth / bottom_depth_m ) %>% 
  mutate(castratio = if_else(castratio > 1, 1, castratio)) 

# 8. Curate the sampling volume data frame

# The volume sampled in each UVP profile and region names/color
load("data/UVP6_sample_volume_5m_20251211.RData")
rm(region_rename) # old version

# Retain only the sample_ids used in the analysis
volumes.sum <- volumes.df %>% 
  # Add the min and max 5m depth bins of the UVP6 cast and add to the metadata
  group_by(sample_id) %>% 
  summarise(min5mDepth = min(Depth_5m),
            max5mDepth = max(Depth_5m),
            totalProfileVolume_m3 = sum(Sampled_volume_m3),
            n5mbins = n(), .groups = "drop") 

# Add volumes to the metadata table
Amund_UVP6_meta <- Amund_UVP6_meta %>% 
  left_join(volumes.sum, by = "sample_id")


# ------------------
# 9. Curate the UVP6 profiles for analysis

# Count the number of Calanus spp images in each UVP6 profile.
# Note that some profiles do not have a Calanus images validated so are excluded here. Many are shallow casts.
df.calanus.count <- all.copepods.uvp6 %>% 
  filter(object_annotation_category %in% c("Calanus",
                                           "side<with visible lipid sac",
                                           "top-bottom<with visible lipid sac")) %>%
  group_by(sample_id) %>% 
  summarise(nCalanusImages = n(), .groups = "drop")

  
valid_profiles <- Amund_UVP6_meta %>% 
  # Exclude some profiles that are shallow or low images, or beyond domain
  left_join(df.calanus.count, by = "sample_id") %>% 
  filter(!(bottom_depth_m < 100 | nCalanusImages < 10 |
           sample_stationid %in% c("RA11","RA58",
                                   "UNG-3", "TCAT1_03")))  


excluded_profiles <- Amund_UVP6_meta %>% 
  left_join(df.calanus.count, by = "sample_id") %>% 
  filter(!(sample_id %in% valid_profiles$sample_id))


# Make sure all metadata tables are for the profiles for analysis only
Amund_UVP6_meta <- Amund_UVP6_meta %>%
  left_join(df.calanus.count, by = "sample_id") %>% 
  filter(sample_id %in% valid_profiles$sample_id)

volumes.df <- volumes.df %>% 
  filter(sample_id %in% valid_profiles$sample_id)


# 10. Add the CTD temperature for the depth the individual was captured
# By default, use the 5 m bin for the CTD match. When not possible, use the 100 m  temperature average. 

# The CTD data temperature and region names/color
load("data/uvp6_associated_ctd_20260320.RData")

Amund_UVP6_meta <- Amund_UVP6_meta %>% 
  # Assign the ctd file name for the profiles and also regions
  left_join(uvp6_ctd_match %>% 
              distinct(sample_id, ctd_filename, region),
            by = join_by(sample_id)) 

# Extract the depths where each image was captured
AA <- all.copepods.uvp6 %>% 
  left_join(uvp6_ctd_match %>% 
              distinct(sample_id, ctd_filename, region),
            by = join_by(sample_id)) %>% 
  select(object_id, object_depth_max, sample_id, ctd_filename, region)

# Step 1: Join the temperature data based on the filename and temp_depth_bin
temp_data_A <- AA %>% 
  # Assign depth bin to match with the ctd data (set to 5m similar to volume)
  mutate(temp_depth_bin = floor(object_depth_max/5)*5) %>% 
  left_join(temp_profiles %>% 
              select(ctd_filename = filename,
                     temp_depth_bin = depth_bin,
                     temperature_degC = temp.mean),
            by = join_by(ctd_filename, temp_depth_bin)) %>% 
  filter(!is.na(temperature_degC))

# Step 2: profiles with no matching temperature data, use the regional averages for the depth layer 
temp_data_B <- AA %>% 
  filter(object_id %notin% temp_data_A$object_id) %>% 
  mutate(temp_depth_bin = floor(object_depth_max/100)*100) %>% 
  # Similar to above but at 100 m interval
  left_join(temp_profiles_100 %>% 
              select(ctd_filename = filename,
                     temp_depth_bin = depth_bin,
                     temperature_degC = temp.mean),
            by = join_by(ctd_filename, temp_depth_bin)) %>% 
  filter(!is.na(temperature_degC))

# Step 3: Use the regional average for 100 m bins
## Calculate regional averages
region_temp_profiles_100 <- temp_profiles_100 %>% 
  left_join(uvp6_ctd_match %>% 
              distinct(ctd_filename, region),
            by = join_by(filename == ctd_filename)) %>% 
  group_by(region, depth_bin) %>% 
  summarise(reg.temp.mean = mean(temp.mean, na.rm = T),
            reg.temp.sd = sd(temp.mean, na.rm = T),
            nprofiles = n(), .groups = "drop")

## Apply regional averages
temp_data_C <- AA %>% 
  filter(object_id %notin% temp_data_A$object_id) %>% 
  filter(object_id %notin% temp_data_B$object_id) %>% 
  mutate(temp_depth_bin = floor(object_depth_max/100)*100) %>% 
  left_join(region_temp_profiles_100 %>% 
              select(region,
                     temp_depth_bin = depth_bin,
                     temperature_degC = reg.temp.mean),
            by = join_by(region, temp_depth_bin))


# Step 4: Merge the temperature data and join with the image dataframes
temp_data_merged <- bind_rows(temp_data_A, temp_data_B, temp_data_C) %>% 
  select(object_id, temperature_degC) %>% 
  mutate(temperature_kelvin = temperature_degC + 273.15) 
rm(AA, temp_data_A, temp_data_B, temp_data_C, region_temp_profiles_100)

# Add temperature both to the Calanus segmentations and all copepods data frames
segmented.images <- segmented.images %>% 
  left_join(temp_data_merged, by = join_by(object_id))
all.copepods.uvp6 <- all.copepods.uvp6 %>%
  left_join(temp_data_merged, by = join_by(object_id))

  
# 11. Add region information -----------------

# Prepare data frame of names and abbreviations for figures + colors
region_rename <- data.frame(
  region = c("Baffin Bay", "Baffin Shelf", 
             "Davis Strait", "Lancaster Sound",
             "North Water Polynya", "Labrador Shelf", 
             "Nares Strait"),
  region.inuktitut = c("Sannirutiup Imavik\n(Baffin Bay)", 
                       "Qikiqtaaluk Shelf\n(Baffin Shelf)",
                       "Qikiqtaaluup Ikinga\n(Davis Strait)", 
                       "Tallurutiup Imanga\n(Lancaster Sound)",
                       "Pikialasorsuaq\n(North Water Polynya)", 
                       "Nunatsiavut Shelf\n(Labrador Shelf)",
                       "Nares Strait"),
  # The shorter version for figures
  region.inuk = c("Sannirutiup Imavik", "Qikiqtaaluk Shelf",
                  "Qikiqtaaluup Ikinga", "Tallurutiup Imanga",
                  "Pikialasorsuaq", "Nunatsiavut Shelf","Nares Strait"),
  # The abbreviated versions
  reg.inuk = c("SanIm","QikSh","QikIk","TalIm","Pikia","NunSh","NarSt"),
  region.color = c("goldenrod2","purple2","turquoise3","forestgreen",
                   "deeppink1","blue1", "sienna2")) %>% 
  # order regions as factors by latitude
  mutate(region.inuktitut = factor(
    region.inuktitut,
    levels = c("Nares Strait",
               "Pikialasorsuaq\n(North Water Polynya)", 
               "Tallurutiup Imanga\n(Lancaster Sound)",
               "Sannirutiup Imavik\n(Baffin Bay)", 
               "Qikiqtaaluup Ikinga\n(Davis Strait)", 
               "Qikiqtaaluk Shelf\n(Baffin Shelf)",
               "Nunatsiavut Shelf\n(Labrador Shelf)")),
    region.inuk = factor(region.inuk,
                         levels = c("Nares Strait",
                                    "Pikialasorsuaq", 
                                    "Tallurutiup Imanga",
                                    "Sannirutiup Imavik", 
                                    "Qikiqtaaluup Ikinga", 
                                    "Qikiqtaaluk Shelf",
                                    "Nunatsiavut Shelf")),
    reg.inuk = factor(reg.inuk,
                      levels = c("NarSt","Pikia","TalIm","SanIm",
                                 "QikIk","QikSh","NunSh"))  ) %>% 
  arrange(region.inuktitut)


Amund_UVP6_meta <- Amund_UVP6_meta %>% 
  left_join(region_rename) %>% 
  select(-region.color) %>%  
  # Add the station_year to mark repeated profiles in the same station
  mutate(station_year = paste(sample_stationid_ed, Year)) 

# 12. Export the curated data files
  save(Amund_UVP6_meta,
       segmented.images, all.copepods.uvp6,
       volumes.df, region_rename, excluded_profiles,
       file = "data/uvp6_CNN_segmentations_morphotraits_20260320.RData")

