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

years = c("1985", "1990", "1995", "2000", "2001", "2005", "2010", "2015", "2020")

years <- list.dirs(datasets_dir,
                   full.names = FALSE, # Now we want only the year, NOT the full path
                   recursive = FALSE)
year_dirs <- list.dirs(datasets_dir,
                       full.names = TRUE,
                       recursive = FALSE)
names(year_dirs) <- years
# Now the list "cropped_rasters_list" is a "named list" 
#----------------------------------------------------------

crop_rasters <- lapply(1:length(years), function(fidx) {
  year_dir <- year_dirs[[fidx]]
  #folder1 = tif_dirs_full_year[folder]
  year_dataset_list <- lapply(year_dir, function(d){
    #    folder2 = folder1[d]
    #Get list of TIF files in each dir
    year_dataset_list = list.dirs(d, full.names = TRUE, recursive = FALSE)
    
    return(year_dataset_list)
  })
  year_dataset_list <- unlist(year_dataset_list)
  if (length(year_dataset_list) > 0) {
    final_list <- lapply(year_dataset_list, function(d) {
      # Get list of TIF files in each dir
      tif_list = list.files(d, pattern = "TIF$",
                            full.names = TRUE, recursive = TRUE)
      if (length(tif_list) > 0) {
        # pass both list of tif files, and containing directory to the cropping function
        # The directory name will be used to name the new, cropped tif file
        
        cropped <- CropDatasets(tif_list, full_area)
        # Textures?
        final <- AddImageTexture(cropped)
        return(final)   
      }
    })
    l = sds(final_list)
    final_stack = app(l, mean, na.rm = TRUE)
    
    #save the images (after cropped, texture, index and mear per year )
    rastname = paste("Full_Area", years[fidx], sep="_")
    rastpath <- file.path(fullarea_dir, paste0(rastname, ".tif"))
    terra::writeRaster(x= final_stack,
                       filename = rastpath, overwrite = TRUE)
    return(final_stack)
  }
})
#}

#? will this give the right name
names(crop_rasters) = paste0("fullarea", years)

#split the raster "list" intp 2 groups by landsat
# the numbers will change depending on the number of images 

crop_rasters_l5 = crop_rasters[as.numeric(years) < 2013]
crop_rasters_l8 = crop_rasters[as.numeric(years) >= 2013]


fullarea = list.files(fullarea_dir, pattern = "tif$",
                         full.names = TRUE)
fullarea <- fullarea[grep(pattern = "Full_Area", x = fullarea)]  #takes only ... by pattern

rast_4_RF_l5 = rast(fullarea[5])
rast_4_RF_l8 = rast(fullarea[9])
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
# rast_4_RF_l5 = crop_rasters$LT05_L2SP_174039_20020228_20211206_02_T1
# rast_4_RF_l8 = crop_rasters$LC08_L2SP_174039_20200418_20200822_02_T1

#should change to...
rast_4_RF_l5 = crop_rasters$fullarea2002
rast_4_RF_l8 = crop_rasters$fullarea2020

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
RFmodel_l5 = Prepare_RF_Model(training_data = training_data_L5, mod_name =  landsat5)
RFmodel_l8 = Prepare_RF_Model(training_data = training_data_L8, mod_name = landsat8)

# get list of names of cropped raster files
tif_cropped = list.files(fullarea_dir, pattern = "tif$",
                         full.names = TRUE)
tif_cropped <- tif_cropped[grep(pattern = "Full_Area", x = tif_cropped)]  #takes only ... by pattern

#can we make a "variable" of the year in the name to compare to so dividing into l5 and l8 isnt with list...

#need to make 2 list of cropped images by landsat to classify with the correct model

tif_cropped_l5 = tif_cropped[as.numeric(years) < 2013]
tif_cropped_l8 = tif_cropped[as.numeric(years) >= 2013]

#'---------------------------------
#' Run classification
#'---------------------------------

classified_rasters_l5 = classified_rasters(tif_cropped = tif_cropped_l5, 
                                           bands = bands_l5, fit = RFmodel_l5, landsat = landsat5)
classified_rasters_l8 = classified_rasters(tif_cropped = tif_cropped_l8, 
                                           bands = bands_l8, fit = RFmodel_l8, landsat = landsat8)
#PlotClassified(tif_cropped, classified_rasters)


# get list of names of classified raster files
tif_classified = list.files(classified_full_dir, pattern = "tif$",
                         full.names = TRUE)
tif_classified_l5 <- tif_classified[grep(pattern = "l5", x = tif_classified)]  #takes only... by pattern
tif_classified_l8 <- tif_classified[grep(pattern = "l8", x = tif_classified)]


buffer500 = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                 layer="area_buffer500_detailed")
yishuv_mask = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                   layer="yishuv_mask")

#crop fullarea image to yishuv
crop_yishuv_l5 = crop_yishuv(tif_cropped = tif_cropped_l5, landsat = landsat5)
crop_yishuv_l8 = crop_yishuv(tif_cropped = tif_cropped_l8, landsat = landsat8)

crop_classified_rasters_l5 =  crop_classified_rasters(tif_classified = tif_classified_l5, landsat = landsat5)
crop_classified_rasters_l8 =  crop_classified_rasters(tif_classified = tif_classified_l8, landsat = landsat8)

tif_crop_classified = list.files(classified_cropped_dir, pattern = "tif$",
                                 full.names = TRUE)

#tiff list of all classified rasters by yishuv
tif_cc_Hazeva <- tif_crop_classified[grep(pattern = "Hazeva", x = tif_crop_classified)]  #takes only... by pattern
tif_cc_Ein_Yahav <- tif_crop_classified[grep(pattern = "Ein_Yahav", x = tif_crop_classified)]  #takes only... by pattern
tif_cc_Paran <- tif_crop_classified[grep(pattern = "Paran", x = tif_crop_classified)]  #takes only... by pattern

#multiband raster of all cropped classified rasters by yishuv
rast_cc_hazeva = rast_cc(tif_cc = tif_cc_Hazeva)
rast_cc_ein_yahav = rast_cc(tif_cc = tif_cc_Ein_Yahav)
rast_cc_paran = rast_cc(tif_cc = tif_cc_Paran)

col = c("gray", "navajowhite1", "lightskyblue1", "dark green")
lev = levels(training_data_L5$ground_type)
#lev1 = levels(c("Light Green houses", "Dark Green Houses", "Orchard", "Ground"))

#plot to pdf all classified rasters by yishuv
pdf(file ="./output/h.pdf", width = 9.5, height = 5)
plot(rast_cc_hazeva, col = col, legend = FALSE)#type = "classes", levels = lev )
dev.off()
pdf(file ="./output/ey.pdf", width = 6, height = 5)
plot(rast_cc_ein_yahav, col = col, legend = FALSE)#type = "classes", levels = lev)
dev.off()
pdf(file ="./output/p.pdf", width = 9, height = 5)
plot(rast_cc_paran, col = col, legend = FALSE)#type = "classes", levels = lev)
dev.off()

# plot to get legend
pdf(file ="./output/p_legend2.pdf", width = 12, height = 5)
plot(rast_cc_paran$Paran_1985_01, col = col,type = "classes", levels = lev, legend = "bottomleft")
sbar(2000, xy= c(700500, 3361000), type="bar", divs=4, label = c(0,1,2), cex = 0.9) #below = "Km",adj = c(-2, -0.1)
#north(type=1)
dev.off()

#turn the rasters into df to plot with ggplot
df_hazeva = raster_to_df(rast_cc_hazeva)
df_hazeva = df_hazeva %>%
  separate(variable, c("yishuv", "year"), "_")
df_ein_yahav = raster_to_df(rast_cc_ein_yahav)
df_ein_yahav = df_ein_yahav %>%
  separate(variable, c("yishuv1", "yishuv2", "year"), "_")%>%
  unite(yishuv, c(yishuv1, yishuv2))
df_paran = raster_to_df(rast_cc_paran)
df_paran = df_paran%>%
  separate(variable, c("yishuv", "year"), "_")

pdf("./output/hazeva_9_n1.pdf", width = 10, height = 8)
ggplot()+
  geom_raster(data = df_hazeva, aes(x=x, y=y, fill = value_c))+
  facet_wrap(~year)+
  scale_fill_manual(name = "LCLU Classes",
                    values = c("Dark PA" = "salmon3",
                               "Light PA" = "lightskyblue1",
                               "Open Ground" = "navajowhite1",
                               "Orchard/Vegetation" = "forestgreen"))+
  coord_fixed(ratio = 1)+
  theme(legend.position="bottom")+
  labs(title = "Hazeva")+
  theme(plot.title=element_text(hjust=0.5))+
  #geom_sf(fill = "transparent", data = b_h) #changes the coords to geo
  scale_x_continuous(breaks = seq(712500, 722500, by = 2000))+
  scale_y_continuous(breaks = seq(3402500,3410000, by = 2000))
dev.off()

pdf("./output/ein_yahav_9_n1.pdf", width = 8, height = 10)
ggplot()+
  geom_raster(data = df_ein_yahav, aes(x=x, y=y, fill = value_c))+
  facet_wrap(~year)+
  scale_fill_manual(name = "LCLU Classes",
                    values = c("Dark PA" = "salmon3",
                               "Light PA" = "lightskyblue1",
                               "Open Ground" = "navajowhite1",
                               "Orchard/Vegetation" = "forestgreen"))+
  coord_fixed(ratio = 1) +
  theme(legend.position="bottom")+
  labs(title = "Ein Yahav")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks = seq(712500, 717500, by = 2500))+
  scale_y_continuous(breaks = seq(3390000,3400000, by = 2500))
dev.off()

pdf("./output/paran_9_n1.pdf", width = 10, height = 9)
ggplot()+
  geom_raster(data = df_paran, aes(x=x, y=y, fill = value_c))+
  facet_wrap(~year)+
  scale_fill_manual(name = "LCLU Classes",
                    values = c("Dark PA" = "salmon3",
                               "Light PA" = "lightskyblue1",
                               "Open Ground" = "navajowhite1",
                               "Orchard/Vegetation" = "forestgreen"))+
  coord_fixed(ratio = 1)+
  theme(legend.position="bottom")+
  labs(title = "Paran")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks = seq(705000, 709000, by = 2000))+
  scale_y_continuous(breaks = seq(3360000,3364000, by = 2000))
dev.off()


frequency_table_hazeva = frequency_table(tif_cc = tif_cc_Hazeva, yishuv = yishuv_n[1])
frequency_table_ein_yahav = frequency_table(tif_cc = tif_cc_Ein_Yahav, yishuv = yishuv_n[2])
frequency_table_paran = frequency_table(tif_cc = tif_cc_Paran, yishuv = yishuv_n[3])


#'---------------------------------
#' Completed
#'---------------------------------
t2 = Sys.time()
elapsed = round(difftime(t2, t0, units = "mins"),2)
print(paste(t2, "-- End process in", elapsed, "minutes"))

