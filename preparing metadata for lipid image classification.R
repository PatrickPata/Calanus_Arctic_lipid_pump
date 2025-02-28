library(dplyr)

# Load table of images with classified lipids
metadata <- read.csv("C:/Users/patri/python_workspace/test_lipid_classification/uvp6_with_segmentations.csv")


# List file names of all images 
fnames <- data.frame(filename = list.files("C:/Users/patri/python_workspace/test_lipid_classification/images_processed_1-4")) %>% 
  left_join(metadata %>% 
              select(filename, label),
            by = "filename") %>% 
  mutate(label = if_else(is.na(label),
                         "cant segment", label))

write.csv(fnames,
          file = "C:/Users/patri/python_workspace/test_lipid_classification/metadata_uvp6_lipid.csv",
          row.names = FALSE)

