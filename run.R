source("./config.R")
source("./functions.R")
source("./rf_functions.R")

#'---------------------------------
#' Start here
#'---------------------------------
t0 = Sys.time()
print(paste(t0, "-- Begin process"))

# Load study areas and Landsat tiles
study_areas = st_read(file.path(GIS_dir, "greenhouses.gpkg"),
                      layer="areas")
# Get all Landsat folders in datasets_dir
tif_dirs_full <- list.dirs(datasets_dir)[-1]


#'---------------------------------
#' Crop Landsat to study areas
#'---------------------------------
# Work on each Landsat dataset and study area separately
crop_rasters <- lapply(study_areas$name, function(sa){
  lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    # Read into stack, and crop
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      # pass both list of tif files, and containing directory to the cropping function
      # The directory name will be used to name the new, cropped tif file
      #
      print(paste("In:", sa, "directory:", d))
      #print(paste0(sa,"_", tif_dirs_full, ".tif")) # can be name of raster?
      study_area <- study_areas[study_areas$name == sa,]
      cropped <- CropDatasets(tif_list, study_area)
      crop_all_layers <- AddImageTexture(cropped)

      #save the cropped images
      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      rastname = paste(sa, datestr, sep="_")
      rastpath <- file.path(cropped_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= crop_all_layers,
                         filename = rastpath, overwrite = TRUE)

      return(crop_all_layers)
    }
  })
})

#'---------------------------------
#' Random Forest classification
#'---------------------------------
# create rasters for classification
# This list holds all raster bands to be used in classification
# for all dates to be examined
classification_shp <- vect(file.path(GIS_dir, "greenhouses.gpkg"),
                           layer="classification_area")
rast_4_RF_list = lapply(tif_dirs_full, function(td) {
  # Mask to classification area
  tif_list <- list.files(td, full.names = TRUE)
  masked_raster <- CropDatasets(tif_list, classification_shp)
  # add texture and veg index bands
  all_bands <- AddImageTexture(masked_raster)
  return(all_bands)
})

names(rast_4_RF_list) <- basename(tif_dirs_full)

#Prepare RF Model using a single raster stack from the rast_4_RF_list
rast_4_RF = rast_4_RF_list$LC08_L2SP_174039_20200418_20200822_02_T1
training_data = CreateTrainingDF(rast_4_RF)

#'---------------------------------
#' Monte Carlo Simulation
#'---------------------------------
# Do multiple runs of Random Forest, each time with different training/test sets

num_mc_runs <- 5  # Change to 100 after the function below works
rf_results_list <- lapply(1:num_mc_runs, function(training_data){
  rf_result <- Prepare_RF_Model_minimal(training_data)
  return(rf_result)
})
# Now rbind the rf_results_list to get a data.frame
# with 4 columns and num_mc_runs rows
rf_results <- do.call(rbind, rf_results_list)
# and show mean and std of each measure over all monte carlo runs
(rf_results_mean <- sapply(rf_results, mean))
(rf_results_sd <- sapply(rf_results, sd))


# Prepare the random forest model
set.seed(12)
RFmodel = Prepare_RF_Model(training_data)

# get list of names of cropped raster files
tif_cropped = list.files(cropped_dir, pattern = "tif$",
                         full.names = TRUE)

#'---------------------------------
#' Run classification
#'---------------------------------
# loop through the cropped raster's and classify them
classified_rasters = lapply(tif_cropped, function(t){
  # The tif_cropped list already has full path to each file
  r = rast(t)
  r = r[[bands]]
  #plot(r)
  rast_classify = ApplyRFModel(r, fit = RFmodel) # classify the raster
  r_split <- strsplit(x=basename(t), split = ".", fixed = TRUE)
  r_split <- unlist(r_split)[1]
  rastname = paste(r_split, "classified", sep="_")
  rastpath <- file.path(classified_dir, paste0(rastname, ".tif"))
  writeRaster(x = rast_classify, filename = rastpath,
              overwrite = TRUE)

  return(rast_classify)
})

PlotClassified(tif_cropped, classified_rasters)
#'---------------------------------
#' Land surface temperature
#'---------------------------------
LST_crop <- lapply(study_areas$name, function(sa){
  lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    # Read into rast, and crop
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {

      print(paste("In:", sa, "directory:", d))
      study_area <- study_areas[study_areas$name == sa,]
      LST_b <- LST_band(tif_list, study_area)
      #print(range(LST_b))

      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      rastname = paste("LST", sa, datestr, sep="_")
      rastpath <- file.path(LST_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= LST_b, filename = rastpath, overwrite = TRUE)
      plot(LST_b, main = paste(rastname, "C", sep = " "))

      return(LST_b)
    }
  })
})


#'---------------------------------
#' Completed
#'---------------------------------
t2 = Sys.time()
elapsed = round(difftime(t2, t0, units = "mins"),2)
print(paste(t2, "-- End process in", elapsed, "minutes"))
