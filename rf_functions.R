

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
  
  # count number of incomplete rows and erases them
  cnt_na <- sum(!complete.cases(extract_points))
  if (cnt_na > 0) {
    print(paste("Number of rows with NA:", cnt_na, "(removing...)"))
    extract_points <- extract_points[complete.cases(extract_points),]
  }
  

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

Prepare_RF_Model <- function(training_data, mod_name) {
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
  model_rds <- file.path(output_dir,  paste(mod_name, "fitted_RF_model.RDS"))
  saveRDS(rfFit, model_rds)
  # Model results:
  cat("\nModel accuracy:\n")
  print(rfFit$results[rownames(rfFit$bestTune),][c("Accuracy", "Kappa")])
  
  # Get and print variable importance
  var_importance <- varImp(rfFit, scale=TRUE)
  cat("\nVariable importance:\n")
  print(var_importance)
  varimp_file <- file.path(output_dir, paste(mod_name,"variable_importance.png"))
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

#classifies the raster takes list of rasters, what bands to use, model and landsat name
classified_rasters = function(tif_cropped, bands, fit, landsat) {
  
  lapply(tif_cropped, function(t){
  # The tif_cropped list already has full path to each file
  r = rast(t)
  r = r[[bands]]
  #plot(r)
  rast_classify = ApplyRFModel(r, fit) # classify the raster
  r_split <- strsplit(x=basename(t), split = ".", fixed = TRUE)
  r_split <- unlist(r_split)[1]
  rastname = paste(r_split, paste0("classified_", landsat), sep="_")
  rastpath <- file.path(classified_full_dir, paste0(rastname, ".tif"))
  writeRaster(x = rast_classify, filename = rastpath,
              overwrite = TRUE)
  
  return(rast_classify)
  })
}


PlotClassified <- function(rast_list, classified_list) {
  # to add to plots
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

#saves the classified images cropped to the ysihuv
crop_classified_rasters = function(tif_classified, landsat) {
  lapply(buffer500$name, function(sa){
    lapply(tif_classified, function(t) {
      r = rast(t)
      yishuv_mask_r = rasterize(yishuv_mask, r) #rasterizes the yishuv and othe polygons
      yishuv_mask_r[yishuv_mask_r ==1] = -999 # changes the polygon value to -999
      print(paste("In:", sa, "directory:", t))
      study_area <- buffer500[buffer500$name == sa,]
      #crop and mask to yishuv out line
      #cropped <- terra::crop(r, study_area)
      masked = terra::mask(r, study_area)
      #classified_mask = terra::mask(masked, yishuv_mask_r, maskvalues = -999)#maskes the area of the yishuv, makes the the raster size bigger with NA's
      #cropped <- terra::crop(classified_mask, study_area)
      cropped <- terra::crop(masked, study_area)
      #save the cropped images
      d_split <- strsplit(x=basename(t), split = "_", fixed = TRUE)
      yearstr <- unlist(d_split)[3]
      monthstr <- unlist(d_split)[4]
      rastname = paste(sa, yearstr, monthstr,paste0("classified_", landsat), sep="_")
      rastpath <- file.path(classified_cropped_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= cropped,
                         filename = rastpath, overwrite = TRUE)
      #plot(classified_mask, main = rastname)
      plot(cropped, main = rastname)
      #return(classified_mask)
      return(cropped)
    })
  })
}

crop_yishuv = function(tif_cropped, landsat) {
  lapply(buffer500$Name, function(sa){
    lapply(tif_cropped, function(t) {
      r = rast(t)
      print(paste("In:", sa, "directory:", t))
      study_area <- buffer500[buffer500$Name == sa,]
      #mask and crop to yishuv out line
      masked = terra::mask(r, study_area)
      cropped <- terra::crop(masked, study_area)
      #save the cropped images
      d_split <- strsplit(x=basename(t), split = "_", fixed = TRUE)
      yearstr <- unlist(d_split)[3]
      yearstr = substr(yearstr,1,nchar(yearstr)-4)
      rastname = paste(sa, yearstr, landsat, sep="_")
      rastpath <- file.path(cropped_yishuv_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= cropped,
                         filename = rastpath, overwrite = TRUE)
      return(cropped)
    })
  })
}
