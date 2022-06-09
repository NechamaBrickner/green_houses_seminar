# Install and load packages
pkg_list = c(
  "getSpatialData",  # download Landsat tiles
  "terra", "raster", # read rasters
  "sf",              # vector layers
  "parallel",        # parallel processing
  "exactextractr",   # extract and aggregate raster values per polygon, with clipping
  "remotes",         # to use github to install packages
  "glcm",            # Greylevel co-location matrix
  "caret","ranger",  # ML models
  "dplyr", "tools"
)

# installs packages that are not installed 
installed_packages <- pkg_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg_list[!installed_packages])
}

# Package loading
lapply(pkg_list, library, character.only = TRUE)

# not using supercells
# # install.packages("remotes")
# # add if not installed, install now
# if(!require(supercells)){
#   remotes::install_github("Nowosad/supercells")
#   library("supercells")
# }

# remotes::install_github("Nowosad/supercells")
# library("supercells")

# creates folder Paths
# -------------------------
# Setup individually depending on local paths in each computer

GIS_dir = "./GIS"
if (!dir.exists(GIS_dir)) {
  dir.create(GIS_dir)
}

#do we need this folder cuz we already have the images
# folder w/ downloaded landsat 
datasets_dir = "./Landsat_datasets"
if (!dir.exists(datasets_dir)) {
  dir.create(datasets_dir)
}


# folder for output
output_dir = "./output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# folder for cropped images
cropped_dir <- "./output/cropped"
if (!dir.exists(cropped_dir)) {
  dir.create(cropped_dir)
}

# folder for cropped images
fullarea_dir <- "./output/fullarea"
if (!dir.exists(fullarea_dir)) {
  dir.create(fullarea_dir)
}

# folder for classified raster's output
classified_full_dir = "./output/Classified_full"
if (!dir.exists(classified_full_dir)) {
  dir.create(classified_full_dir)
}

classified_cropped_dir = "./output/Classified_cropped"
if (!dir.exists(classified_cropped_dir)) {
  dir.create(classified_cropped_dir)
}

# # folder for  LST output
# LST_dir = "./output/LST"
# if (!dir.exists(LST_dir)) {
#   dir.create(LST_dir)
# }
# 
# # folder for  LST output
# albedo_dir = "./output/albedo"
# if (!dir.exists(albedo_dir)) {
#   dir.create(albedo_dir)
# }

landsat5 = "l5"
landsatl8 = "l8"

# Bands to be used in the RF model
bands_l5=c("blue","NIR","SWIR1", "variance", "NDVI", "BSI", "NDBI")
bands_l8=c("green", "NIR","SWIR2", "variance", "NDVI", "BSI", "NDBI")
