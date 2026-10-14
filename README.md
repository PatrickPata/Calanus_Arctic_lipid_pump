This is the repository for the scripts associated with the manuscript "Calanus Copepods are as Important as Marine Snow in Arctic Vertical Carbon flux" by Pata et al. (2026) submitted to Global Biogeochemical Cycles. (Full citation to follow)

We provide most of the curated datasets to run the analysis and plotting script files named 4-*.Rmd to 13-*.Rmd. The data provided as csv files are generated and organized from script files 0-*.R to 3-*.R. You may skip to script file 4-*.Rmd if you have the necessary data files. After cloning or forking this repository, please create a folder named "data/open_data_csv" and copy the data files that you can download from this Zenodo repository: (https://doi.org/10.5281/zenodo.23378189). 

If you are interested specifically in the sections that use the CTD, raw UVP6 images, and Hydrobios net data, you need to download or get access to these datasets as indicated below. Figures 2, 3a, 3d, 4, and 5 can be generated with the contents of the open_data_csv folder.

The cruise datasets from the CCGS Amundsen are accessible through the Canadian Integrated Ocean Observation System catalogue (https://catalogue.amundsenscience.com). 

The raw copepod image data and particle size distributions are available from the EcoTaxa (https://ecotaxa.obs-vlfr.fr/) and EcoPart (https://ecopart.obs-vlfr.fr/) packages with following project IDs: 5108, 10101, 14844, 14859, 17498, 18084. Please contact Frédéric Maps, Université Laval (frederic.maps@bio.ulaval.ca) if you would like to request access to the raw image files or EcoTaxa projects.

The Hydrobios net data are available upon request from Fisheries and Oceans Canada (DFO). A metadata file named hydrobios_net_samples_metadata_20261014.csv contains the list of net sampling stations used in this study. Please contact  Monika Pućko, Freshwater Institute, Fisheries and Oceans Canada (monika.pucko@dfo-mpo.ca) to access the net taxonomy data from the DFO KEBABB program.

The trained CNN image segmentation models are large files and available upon request (.pth and .ptk). We provide a metadata list of image files used for the CNN image segmentation training. The raw Calanus image files (the same files downloaded from the EcoTaxa project) and the manually segmented mask files are also available upon request. The CNN model scripts for UVP6 image data can be found in a branch of the CNN segmentation model for LOKI image data. The scripts are accessible from https://github.com/PatrickPata/copepods-lipid-content/tree/patrick_dev. Please contact Frédéric Maps or Patrick Pata, Université Laval (patrick.pata.1@ulaval.ca) to request any of these files.

Please contact patrick.pata.1@ulaval.ca for any questions and assistance regarding the scripts and data analysis. 
