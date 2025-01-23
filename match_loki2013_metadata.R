# Match LOKI images segmentation to metadata

library(tidyverse)


`%notin%` <- Negate(`%in%`)

# The LOKI2013 data
meta <- read.csv("C:/Users/patri/Downloads/export_12063_20250122_1820/ecotaxa_export_12063_20250122_1820.tsv",
                 sep = '\t', header = TRUE)


# # Only copepods with visible lipid sac (subset for prosome segmentation?)
# loki2 <- read.table("../../Laval-imaging-analysis/LOKI_for_prosome_segmentation/ecotaxa_export_2331_20241009_1551.tsv", sep = '\t', header = TRUE)


# Actually the JPR paper uses 2216 images which are list in this file:
loki <- read.table("data/ecotaxa_table_JPR-2023-061.tsv",
                   sep = "\t", header = TRUE)
loki <- loki[-1,]

# Can't really match by date and time because these are formatted differently
# AA <- loki %>% 
#   select(object_id_sub = object_id,
#          object_date, object_time) %>% 
#   left_join(meta %>% 
#               select(object_id_all = object_id,
#                      object_date, object_time))

zz <- meta %>% 
  mutate(object_id = str_squish(object_id)) %>% 
  filter(object_id %in% loki$object_id)

AA <- loki %>% 
  filter(object_id %notin% str_squish(meta$object_id))
