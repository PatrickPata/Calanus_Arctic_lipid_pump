# Moving LOKI image files by isolating the copepod images with lipids
# Created by Patrick Pata
# Created on October 8, 2024

library(tidyverse)
`%notin%` <- Negate(`%in%`)

setwd("C:/Laval_Postdoc/Laval-imaging-analysis/LOKI images/LOKI images/")

# This lists more files and the polygons for the segmentations:
segmentation.file <- read.csv("data and readme/copepod_segmentation.csv")

# This file contains more metadata and the is_valid column
prediction.file <- read.csv("data and readme/prediction_results.csv")

prediction.file %>% 
  group_by(is_valid) %>% 
  count()


# Set your input and target directories, and create the vector with the names of the files you want to copy (no need to make it a dataframe)
inputdir  <- "images/"  
targetdir <- "copepod_lipid_images/"  


# list files to copy
filestocopy <- paste0(inputdir,prediction.file$filename)


# Copy the files to the target folder
sapply(filestocopy, function(x) file.copy(from=x, to=targetdir, copy.mode = TRUE))



# Remove segementation files in this list of files not used in the model
# which files don't match
notused <- segmentation.file %>%
  filter(filename %notin% prediction.file$filename)
sapply(paste0('copepod_segmentations/',notused$filename), 
       function(x) file.remove(from=x))


