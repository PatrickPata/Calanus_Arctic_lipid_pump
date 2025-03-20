library(dplyr)

# Load table of images with classified lipids
metadata <- read.csv("C:/Users/patri/python_workspace/copepods-lipid-content/prediction_outputs/model_resnet34_lipidUVP6_deamundp1/prediction_lipids_20250317.csv") %>% 
  mutate(label = "lipid sac") %>% 
  select(filename, label)


# List file names of all images 
fnames <- data.frame(filename = list.files("C:/Users/patri/python_workspace/uvp6_lipid_sac_classification/data/images_processed")) %>% 
  left_join(metadata,
            by = "filename") %>% 
  mutate(label = if_else(is.na(label),
                         "cant segment", label)) %>% 
  mutate(dataset = if_else(str_extract(filename, "^.{4}") == "2021",
                           "DarkEdge2021","Amundsen2023")) 

write.csv(fnames,
          file = "C:/Users/patri/python_workspace/uvp6_lipid_sac_classification/metadata_uvp6_lipid.csv",
          row.names = FALSE)

