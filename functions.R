
CropDatasets <- function(tif_list, study_area) {
  # Read list of TIF files into stack
  # Crop to extent of testing polygons
  # ---------------------------
  # Check whether L5 or L8
  
    if (length(grep(pattern = "LT05", tif_list, fixed = TRUE)) > 0) {
      #select wanted bands landsat 5
      tif_list_05 <- tif_list[grep(pattern="LT05_", x=tif_list)]
      #tif_list_05 <- tif_list_05[grep(pattern="_SR_", x=tif_list_05)]
      tif_list_05 <- tif_list_05[grep(pattern = "B1|B2|B3|B4|B5|B7",
                                      x = tif_list_05)]  
      tif_stk <- rast(tif_list_05)
    } else {
      #select wanted bands landsat 8
      tif_list_08 <- tif_list[grep(pattern="LC08_", x=tif_list)]
      #tif_list_08 <- tif_list_08[grep(pattern="_SR_", x=tif_list_08)]
      # Do not use B1 (aerosol band)
      tif_list_08 <- tif_list_08[grep(pattern = "B2|B3|B4|B5|B6|B7",
                                      x = tif_list_08)]
      tif_stk <- rast(tif_list_08)
   }
    
  names(tif_stk) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
  cropped <- terra::crop(tif_stk, study_area)
  cropped = terra::mask(cropped, study_area) 
  
  
  #rescale factor for refletacne 
  # landsat 8 https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1619_Landsat-8-Collection2_Level-2_Science-Product-Guide-v3.pdf
  # landsat 5 https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1618_Landsat-4-7_C2-L2-ScienceProductGuide-v4.pdf
  cropped = cropped*0.0000275 - 0.2
  return(cropped)
}


AddImageTexture <- function(cropped) {
  # Run glcm() function to create image texture raster
  # Choose one band for glcm, i.e. green
  # Create texture bands from only one spectral band (texture bands from other bands will be almost the same)
  texture <- glcm(raster(cropped$green), #converting the green band to raster format to work in glcm 
                  statistics =  c('variance', 'contrast'),
                  na_opt = "ignore")
  names(texture) <- c("variance","contrast") 
  # -------------------------------
  ndvi <- ((cropped$NIR - cropped$red) / (cropped$NIR + cropped$red))
  bsi =  (((cropped$SWIR1 + cropped$red) - (cropped$NIR + cropped$blue))/(cropped$SWIR1 +cropped$red) +(cropped$NIR+cropped$blue)) #bare soil index
  ndbi = ((cropped$SWIR1 - cropped$NIR) / (cropped$SWIR1 + cropped$NIR)) #Normalized Difference Built-up Index
  index = c(ndvi, bsi, ndbi)
  names(index) <- c("NDVI", "BSI", "NDBI")
  # -------------------------------
  # Add to "cropped"
  # Convert texture bands back to terra "SpatRaster"
  all_layers <- c(cropped, rast(texture), index)
 
  return(all_layers)
}



rast_cc = function(tif_cc){
  #gets name of pic with out .tif at end and ./croppped/ at begining
  name = substr(tif_cc,1,nchar(tif_cc)-18)
  name = substr(name, 29, nchar(name))
  #take the list of files and turn to multiband raster
  r_tif_cc = rast(tif_cc)
  names(r_tif_cc) = name # give each band the name from name
  return(r_tif_cc)
}

#turns raster in to dataframe so it can be plotted with ggplot
raster_to_df = function(rast_to_df) { 
  df = as.data.frame(rast_to_df, xy = TRUE) %>%
    melt(id.vars = c("x", "y")) %>%
    mutate(value_c = value)
  
  df$value_c = as.character(df$value_c)
  df["value_c"][df["value_c"] == 1] = "Dark PA"
  df["value_c"][df["value_c"] == 2] = "Open Ground"
  df["value_c"][df["value_c"] == 3] = "Light PA"
  df["value_c"][df["value_c"] == 4] = "Orchard/Vegetation"
  return(df)
}

#create a table with the frequency of each ground type in every raster, takes raster list and yishuv
frequency_table = function(tif_cc, yishuv){
  #gets name of pic with out .tif at end and ./croppped/ at begining
  name = substr(tif_cc,1,nchar(tif_cc)-18)
  name = substr(name, 29, nchar(name))
  # df to join with nams of each raster
  num = 1:length(name)
  df = data.frame(num, name)
  #take the list of files and turn to multiband raster
  r_tif_cc = rast(tif_cc)
  names(r_tif_cc) = name # give each band the name from name
  #plot(hazeva_r)
  ft = as.data.frame(freq(r_tif_cc)) # calculate the frequency of each land type per band in a dataframe
  ft = ft %>%
    left_join(df, by = c("layer" = "num"))%>% # join with df of raster names
    group_by(layer) %>%
    mutate(porportion = count/sum(count)*100) %>% #add percentage of each land type
    select(name, everything())
  
  table_path <- file.path(output_dir, paste0("frequency_table_1", yishuv, ".csv"))
  write.csv(ft, table_path)

  return(ft)
}

