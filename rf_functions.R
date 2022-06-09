
# Random Forest functions based of of klils

# CropRast_4_RF <- function(landsat_dir){
#   # "landsat_dir" is input folder with the tifs to make the list
#   # take only the tifs with "_SR_" in name
#   # select bands to take
#   # create a raster stack with the wanted bands
#   # give the bands names by color/wavelength 
#   tif_list_RF = list.files(landsat_dir, pattern = "TIF$", full.names = TRUE)
#   tif_list_RF <- tif_list_RF[grep(pattern="_SR_", x=tif_list_RF)]
#   tif_list_RF <- tif_list_RF[grep(pattern = "B1|B2|B3|B4|B5|B6|B7",
#                                   x = tif_list_RF)]
#   # create terra::SpatRaster (stack)
#   tif_stk_RF <- rast(tif_list_RF)
#   names(tif_stk_RF) <- c("aerosol", "blue", "green", "red",
#                       "NIR", "SWIR1", "SWIR2") 
#   # load shpfile of classification area
#   classification_shp <- vect(file.path(GIS_dir, "greenhouses.gpkg"),
#                              layer="classification_area")
#   
#   # crop and mask the raster with the shpfile
#   crop_rast <- terra::crop(tif_stk_RF, classification_shp)
#   masked_rast <- terra::mask(crop_rast, classification_shp)
#   
#   return(masked_rast)
# }
# 
# 
# AddAllBands <- function(raster) {
#   # create glcm texture bands to the green band of the raster
#     # to use glcm the band raster need to be in raster format and not terra-rast
#   # texture bands are variance and second moment
#   # give the texture bands names
#   # create an NDVI band
#   # give it a name
#   texture = glcm(raster(raster$green), 
#                  statistics = c('variance','contrast','dissimilarity'), 
#                  na_opt = "ignore")
#   names(texture) <- c("variance", "contrast","dissimilarity") 
#   ndvi = (raster$NIR - raster$red)/(raster$NIR + raster$red)
#   names(ndvi) = "NDVI"
#   savi = 1.5*((raster$NIR - raster$red)/(raster$NIR + raster$red + 0.5))
#   names(savi) = "SAVI"
#   
#   # combine all the bands to 1 raster
#     # the texture bands need to be converted to terra-rast format
#   allbands <- c(raster, rast(texture), ndvi, savi)
#   #allbands <- c(raster, rast(texture), ndvi)
#   return(allbands)
# }



CreateTrainingDF <- function(r, training_data, bands){
  # parameters in the function:
    # r - raster being used to extract values
    # training data - point layer that gets the raster values 
    # bands - subset of bands that will be used (no need for all bands)
  
  # "tif" is the chosen raster stack to be used *for training* 
  # takes training data points from a layer

  training_data = training_data %>%
    filter(Ground_Typ != "Water")
  training_data <- vect(training_data)
  
  # selects wanted bands to build the model
  train_bands <- r[[bands]]  
  extract_points = terra::extract(train_bands, training_data,
                                  method = "simple")
  
  extract_points$ground_type = factor(training_data$Ground_Typ)
  extract_points = select(extract_points, -ID)
  
  # # get rid of the geometry field in training_data table
  # # join the training data with the extract_points
  # training_data = st_drop_geometry(training_data)
  # training_data = left_join(training_data, extract_points, by = c("Id" = "ID"))
  # 
  # # create a table with the names of classes and the number for each class as a factor
  # class_name = unique(training_data$Ground_Typ)
  # class_factor = 1:length(class_name)
  # class = as.data.frame(cbind(class_name, class_factor))
  # class$class_factor = factor(class$class_factor, labels = class_name)
  # 
  # join the training_data table with the factor data 
  # get rid of columns that aren't needed
  # training_data = full_join(training_data, class, by = c("Ground_Typ" = "class_name"))
  # training_data = select(training_data, -Ground_Typ, -Area)
  
  # count number of incomplete rows and erases them
  cnt_na <- sum(!complete.cases(extract_points))
  if (cnt_na > 0) {
    print(paste("Number of rows with NA:", cnt_na, "(removing...)"))
    extract_points <- extract_points[complete.cases(extract_points),]
  }
  # get rid of rows with NA
  # training_data = na.omit(training_data)

  return(extract_points)
}

###############################

# Function to run multiple times in Monte Carlo simulation
Prepare_RF_Model_minimal = function(training_data){
  # This function:
  # (1) prepares a new random sampling of train/test
  # (2) prepares RF model with train data, and saves accuracy and Kappa
  # (3) predicts on the test data, and saves accuracy and Kappa
  # (4) returns a one row dataframe with above 4 values
  # This will be a copy of Prepare_RF_Model() but without the Variable Imp.
  rf_result = data.frame("train_accuracy" = numeric(),
                         "train_kappa" = numeric(),
                         "test_accuracy" = numeric(),
                         "test_kappa" = numeric())
  rfGrid <- expand.grid(mtry = 2:5,         # Number of variables at each split
                        splitrule = "gini",  # How to decide when to split
                        min.node.size = 2:5  # How deep each tree
  )
  
  # Limit how many K-folds and how many retries 
  rfControl <- trainControl(                # 10-fold CV, 3 repeats
    method = "repeatedcv",
    number = 10,
    repeats = 4
  )
  
  # Split train/test
  train_idx <- createDataPartition(
    y = training_data$ground_type,
    p = .75,
    list = FALSE
  )
  train_df <- training_data[train_idx,]
  test_df <- training_data[-train_idx,]
  
  
  rfFit <- train(ground_type ~ ., data = train_df, 
                 method = "ranger", 
                 trControl = rfControl, 
                 verbose = TRUE, 
                 tuneGrid = rfGrid,
                 preProcess=c("center", "scale"),
                 importance = "permutation" 
                 # -----Note:-----
                 # discuss whether to use "impurity" or "permutation"
  )
  
  
  rf_result[1,'train_accuracy'] <- rfFit$results[rownames(rfFit$bestTune),]["Accuracy"]
  rf_result[1,'train_kappa'] <- rfFit$results[rownames(rfFit$bestTune),]["Kappa"]
  
  # Apply on test data, and show confusion matrix 
  rfPred <- predict(rfFit, newdata = test_df)
  con.mat <- confusionMatrix(rfPred, reference = test_df$ground_type)
  rf_result[1,'test_accuracy'] <-con.mat$overall["Accuracy"]
  rf_result[1,'test_kappa'] <-con.mat$overall["Kappa"]
 
  # and without plotting or printing
  # .... fill in here ....
  return(rf_result)
}


##################################

Prepare_RF_Model <- function(training_data) {
  # Limit number of variables that will be tried in train()
  # To limit how many variable comgination are tried, We can set either:
  # set tuneGrid (specify values for each parameter), or
  # set tuneLength (specify how many options for all variables)
  # Here is with tuneGrid:
  # these numbers can be changed to get a better modle???
  rfGrid <- expand.grid(mtry = 2:5,         # Number of variables at each split
                        splitrule = "gini",  # How to decide when to split
                        min.node.size = 2:5  # How deep each tree
  )
  # This grid gives a total of 28 combinations of parameters??? not 24
  
  # Limit how many K-folds and how many retries 
  rfControl <- trainControl(                # 10-fold CV, 3 repeats
    method = "repeatedcv",
    number = 10,
    repeats = 4
  )
  
  # Split train/test
  train_idx <- createDataPartition(
    y = training_data$ground_type,
    p = .75,
    list = FALSE
  )
  train_df <- training_data[train_idx,]
  test_df <- training_data[-train_idx,]
  
  ## Parallel processing
  ## Use 1/2 of the cores
  # ncores = parallel::detectCores() / 2
  # clust <- makeCluster(ncores)
  # registerDoParallel(clust)
  rfFit <- train(ground_type ~ ., data = train_df, 
                 method = "ranger", 
                 trControl = rfControl, 
                 verbose = TRUE, 
                 tuneGrid = rfGrid,
                 preProcess=c("center", "scale"),
                 importance = "permutation" 
                 # -----Note:-----
                 # discuss whether to use "impurity" or "permutation"
  )
  
  # Save model
  model_rds <- file.path(output_dir, "fitted_RF_model.RDS")
  saveRDS(rfFit, model_rds)
  # Model results:
  cat("\nModel accuracy:\n")
  print(rfFit$results[rownames(rfFit$bestTune),][c("Accuracy", "Kappa")])
  
  # Get and print variable importance
  var_importance <- varImp(rfFit, scale=TRUE)
  cat("\nVariable importance:\n")
  print(var_importance)
  varimp_file <- file.path(output_dir, "variable_importance.png")
  vip <- ggplot(var_importance)
  ggsave(varimp_file, plot = vip)
  #png(varimp_file)
  #plot(var_importance, main="Variable Importance")
  #dev.off()
  # stopCluster(clust)
  
  # Apply on test data, and show confusion matrix 
  rfPred <- predict(rfFit, newdata = test_df)
  con.mat <- confusionMatrix(rfPred, reference = test_df$ground_type)
  cat("\nTest accuracy:\n") 
  print(con.mat$overall[c("Accuracy", "Kappa")])
  cat("\nConfusion matrix:")
  print(con.mat$table)
  return(rfFit)
}

# #to check
# rf_model2$bestTune
# rf_model2$results

#it always crashes!!
ApplyRFModel <- function(r, fit) {
  # Apply model to rasters
  
  #predict for the new point layer without solar panels and water and with ground_d
  r_predict <- terra::predict(object = r, model = fit,
                              factors = c("Orchard", "Ground", "Light_Green_House", 
                                          "Dark_Green_House"),
                              na.rm = TRUE) 
  
  return(r_predict)
}

PlotClassified <- function(rast_list, classified_list) {
  # to add to plots
  #colors = c("gray", "yellow", "cyan", "dark green", "black", "blue")
  colors = c("gray", "yellow", "cyan", "dark green", "blue")
  par(mfrow = c(2,1))
  lapply(seq_along(rast_list), function(i){
    rst = rast(rast_list[[i]])
    cls = classified_list[[i]]
  
    # plotRGB(rst,
    #         r=3, g=2, b=1, main="True color")
    plot(rst$green)
    plot(cls,
        col = colors,
        main = "Classified")
    #dev.off()
  })
}