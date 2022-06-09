source("./config.R")
source("./functions.R")
source("./rf_functions.R")

#'---------------------------------
#' Start here
#'---------------------------------
t0 = Sys.time()
print(paste(t0, "-- Begin process"))

# Load study areas and Landsat tiles
full_area = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                      layer="classification_area")
# Get all Landsat folders in datasets_dir
tif_dirs_full <- list.dirs(datasets_dir)[-1]


#'---------------------------------
#' Crop Landsat to full area
#'---------------------------------
# Work on each Landsat dataset separately
crop_rasters <- lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      # pass both list of tif files, and containing directory to the cropping function
      # The directory name will be used to name the new, cropped tif file
      # 
      cropped <- CropDatasets(tif_list, full_area)
      crop_all_layers <- AddImageTexture(cropped)
      
      #save the cropped images
      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      datestr = paste(substring(datestr, 1, 4), substring(datestr, 5, 6), substring(datestr, 7,8), sep = "_")
      rastname = paste("full_area", datestr, sep="_")
      rastpath <- file.path(cropped_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= crop_all_layers,
                         filename = rastpath, overwrite = TRUE)
     
      return(crop_all_layers)
    }
})

names(crop_rasters) <- basename(tif_dirs_full) #gives the name of the image by the date...

#split the raster "list" intp 2 groups by landsat
# the numbers will change depending on the number of images 
#computer at the lab 
crop_rasters_l5 = crop_rasters[8:14]
crop_rasters_l8 = crop_rasters[1:7]

#my computer
crop_rasters_l5 = crop_rasters[3:4]
crop_rasters_l8 = crop_rasters[1:2]
#'---------------------------------
#' Random Forest classification
#'---------------------------------
# crop_rasters list holds *all* dates with 9 bands each
# Select only 1 for RF

#load the training poinys for each model
training_data_l5 = st_read(file.path(GIS_dir,"greenhouses.gpkg"),
                           layer="cp_L5")
training_data_l8 = st_read(file.path(GIS_dir,"greenhouses.gpkg"),
                           layer="cp2")

#Prepare RF Model using a single raster stack from the rast_4_RF_list
#the image for landsat5 is from 28_02_2002
#the image for landsat8 is from 18_04_2020 
rast_4_RF_l5 = crop_rasters$LT05_L2SP_174039_20020228_20211206_02_T1
rast_4_RF_l8 = crop_rasters$LC08_L2SP_174039_20200418_20200822_02_T1


#create the training data for each model
training_data_L5 = CreateTrainingDF(r = rast_4_RF_l5, training_data = training_data_l5, bands = bands_l5)
training_data_L8 = CreateTrainingDF(r = rast_4_RF_l8, training_data = training_data_l8, bands = bands_l8 )

##############################
#' 
#' #'---------------------------------
#' #' Monte Carlo Simulation
#' #'---------------------------------
#' # Do multiple runs of Random Forest, each time with different training/test sets
#' 
#' num_mc_runs <- 1  # Change to 100 after the function below works
#' 
#' rf_results_list_l5 <- lapply(1:num_mc_runs, function(training_data=training_data_L5){
#'   rf_result <- Prepare_RF_Model_minimal(training_data= training_data_L5)
#'   return(rf_result)
#' })
#' 
#' # Now rbind the rf_results_list to get a data.frame
#' # with 4 columns and num_mc_runs rows
#' rf_results_l5 <- do.call(rbind, rf_results_list_l5)
#' # and show mean and std of each measure over all monte carlo runs
#' (rf_results_l5_mean <- sapply(rf_results_l5, mean))
#' (rf_results_l5_sd <- sapply(rf_results_l5, sd))
#' 
#' 
#' rf_results_list_l8 <- lapply(1:num_mc_runs, function(training_data=training_data_L8){
#'   rf_result <- Prepare_RF_Model_minimal(training_data= training_data_L8)
#'   return(rf_result)
#' })
#' 
#' # Now rbind the rf_results_list to get a data.frame
#' # with 4 columns and num_mc_runs rows
#' rf_results_l8 <- do.call(rbind, rf_results_list_l8)
#' # and show mean and std of each measure over all monte carlo runs
#' (rf_results_l8_mean <- sapply(rf_results_l8, mean))
#' (rf_results_l8_sd <- sapply(rf_results_l8, sd))


#####################################


# Prepare the random forest model
set.seed(12)
RFmodel_l5 = Prepare_RF_Model(training_data = training_data_L5)
RFmodel_l8 = Prepare_RF_Model(training_data = training_data_L8)

# get list of names of cropped raster files
tif_cropped = list.files(cropped_dir, pattern = "tif$",
                         full.names = TRUE)
tif_cropped <- tif_cropped[grep(pattern = "full_area", x = tif_cropped)]  #takes only ... by pattern

#can we make a "variable" of the year in the name to compare to so dividing into l5 and l8 isnt with list...

#need to make 2 list of cropped images by landsat to classify with the correct model
#
tif_cropped_l5 = tif_cropped[c(1,3,5,7,9,11,13)]
tif_cropped_l8 = tif_cropped[c(15,17,19,21,23,24,27)]

#'---------------------------------
#' Run classification
#'---------------------------------
# loop through the cropped raster's and classify them 
classified_rasters_l5 = lapply(tif_cropped_l5, function(t){
  # The tif_cropped list already has full path to each file
  r = rast(t)
  r = r[[bands]]
  #plot(r)
  rast_classify = ApplyRFModel(r, fit = RFmodel_l5) # classify the raster
  r_split <- strsplit(x=basename(t), split = ".", fixed = TRUE)
  r_split <- unlist(r_split)[1]
  rastname = paste(r_split, "classified_l5", sep="_")
  rastpath <- file.path(classified_dir, paste0(rastname, ".tif"))
  writeRaster(x = rast_classify, filename = rastpath,
              overwrite = TRUE)
  
  return(rast_classify)
})

classified_rasters_l8 = lapply(tif_cropped_l8, function(t){
  # The tif_cropped list already has full path to each file
  r = rast(t)
  r = r[[bands]]
  #plot(r)
  rast_classify = ApplyRFModel(r, fit = RFmodel_l8) # classify the raster
  r_split <- strsplit(x=basename(t), split = ".", fixed = TRUE)
  r_split <- unlist(r_split)[1]
  rastname = paste(r_split, "classified_l8", sep="_")
  rastpath <- file.path(classified_dir, paste0(rastname, ".tif"))
  writeRaster(x = rast_classify, filename = rastpath,
              overwrite = TRUE)
  
  return(rast_classify)
})
#PlotClassified(tif_cropped, classified_rasters)


# get list of names of classified raster files
tif_classified = list.files(classified_dir, pattern = "tif$",
                         full.names = TRUE)
tif_classified_l5 <- tif_classified[grep(pattern = "classified_l5", x = tif_classified)]  #takes only... by pattern
tif_classified_l8 <- tif_classified[grep(pattern = "classified_l8", x = tif_classified)]

#'---------------------------------
#' albedo band
#'---------------------------------
albedo = lapply(tif_cropped, function(t){
  r = rast(t)
  albedo_b = albedo_band(cropped = r)
  d_split <- strsplit(x=basename(t), split = ".", fixed = TRUE)
  datestr <- unlist(d_split)[1]
  rastname = paste("albedo", datestr, sep="_")
  rastpath <- file.path(albedo_dir, paste0(rastname, ".tif"))
  terra::writeRaster(x= albedo_b, filename = rastpath, overwrite = TRUE)
  plot(albedo_b, main = rastname)
  
  return(albedo_b)
})


#'---------------------------------
#' Land surface temperature
#'---------------------------------
LST_crop <- lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    # Read into rast, and crop
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      LST_b <- LST_band(tif_list, full_area)
      #print(range(LST_b))

      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      datestr = paste(substring(datestr, 1, 4), substring(datestr, 5, 6), substring(datestr, 7,8), sep = "_")
      rastname = paste("LST", datestr, "full_area", sep="_")
      rastpath <- file.path(LST_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= LST_b, filename = rastpath, overwrite = TRUE)
      plot(LST_b, main = paste(rastname, "C", sep = " "))
      
      return(LST_b)
    }
  })

buffer500 = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                 layer="area_buffer500_detailed")
yishuv_mask = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                   layer="yishuv_mask")

#loop to crop the classified images to the study area and mask out the yishuv
crop_classified_rasters_l5 <- lapply(buffer500$name, function(sa){
  lapply(tif_classified_l5, function(t) {
    r = rast(t)
    yishuv_mask_r = rasterize(yishuv_mask, r) #rasterizes the yishuv and othe polygons
    yishuv_mask_r[yishuv_mask_r ==1] = -999 # changes the polygon value to -999
    print(paste("In:", sa, "directory:", t))
    study_area <- buffer500[buffer500$name == sa,]
    #crop and mask to yishuv out line
    cropped <- terra::crop(r, study_area)
    masked = terra::mask(r, study_area)
    classified_mask = terra::mask(masked, yishuv_mask_r, maskvalues = -999)#maskes the area of the yishuv, makes the the raster size bigger with NA's  
    #save the cropped images
    d_split <- strsplit(x=basename(t), split = "_", fixed = TRUE)
    datestr <- unlist(d_split)[3]
    rastname = paste(sa, datestr,"classified_l5", sep="_")
    rastpath <- file.path(cropped_dir, paste0(rastname, ".tif"))
    terra::writeRaster(x= classified_mask,
                       filename = rastpath, overwrite = TRUE)
    plot(classified_mask, main = rastname)
    return(classified_mask)
  })
})

#loop to crop the classified images to the study area and mask out the yishuv
crop_classified_rasters_l8 <- lapply(buffer500$name, function(sa){
  lapply(tif_classified_l8, function(t) {
    r = rast(t)
    yishuv_mask_r = rasterize(yishuv_mask, r) #rasterizes the yishuv and othe polygons
    yishuv_mask_r[yishuv_mask_r ==1] = -999 # changes the polygon value to -999
    print(paste("In:", sa, "directory:", t))
    study_area <- buffer500[buffer500$name == sa,]
    #crop and mask to yishuv out line
    cropped <- terra::crop(r, study_area)
    masked = terra::mask(r, study_area)
    classified_mask = terra::mask(masked, yishuv_mask_r, maskvalues = -999)#maskes the area of the yishuv, makes the the raster size bigger with NA's  
    #save the cropped images
    d_split <- strsplit(x=basename(t), split = "_", fixed = TRUE)
    datestr <- unlist(d_split)[3]
    rastname = paste(sa, datestr,"classified_l8", sep="_")
    rastpath <- file.path(cropped_dir, paste0(rastname, ".tif"))
    terra::writeRaster(x= classified_mask,
                       filename = rastpath, overwrite = TRUE)
    plot(classified_mask, main = rastname)
    return(classified_mask)
  })
})


tif_crop_classified = list.files(cropped_dir, pattern = "tif$",
                                 full.names = TRUE)
tif_crop_classified <- tif_crop_classified[grep(pattern = "classified", x = tif_crop_classified)]  #takes only... by pattern


#gets name of pic with out .tif at end and ./croppped/ at begining
name_yishuv = substr(tif_crop_classified,1,nchar(tif_crop_classified)-4)
name_yishuv = substr(name_yishuv, 11, nchar(name_yishuv))

# df to join with nams of each raster
num = 1:length(name_yishuv)
names = data.frame(num, name_yishuv)


# frequency_table = lapply(tif_crop_classified, function(t){
#   r = rast(t)
#   ft = (freq(r))
#   return(ft)
# })
# 
# names(frequency_table) <- basename(tif_crop_classified)


classified_y = rast(tif_crop_classified)
names(classified_y) = name_yishuv
plot(classified_y)

frequency_table = as.data.frame(freq(classified_y)) # calculate the frequency of each land type per band in a dataframe
frequency_table = frequency_table %>%
  left_join(names, by = c("layer" = "num"))%>% # join with df of raster names
  group_by(layer) %>%
  mutate(porportion = count/sum(count)*100)%>%#add percentage of each land type
  select(name_yishuv, everything())


# frequency_table = sapply(tif_crop_classified, function(t){
#   r = rast(t)
#   ft = freq(r)
#   return(ft)
# })

#'---------------------------------
#' Completed
#'---------------------------------
t2 = Sys.time()
elapsed = round(difftime(t2, t0, units = "mins"),2)
print(paste(t2, "-- End process in", elapsed, "minutes"))



