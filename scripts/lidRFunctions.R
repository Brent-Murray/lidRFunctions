#' lidR Functions
#' This script has useful functions when working with LiDAR Data sets. This script
#' will continually be updated with new and useful functions. The functions are
#' generally well commented and easily understood. There is a requirement to 
#' have some basic LiDAR theory as well as an understanding of R to work with these
#' funcitons.
#' 
#' Created By: Brent Murray


# List of Packages
packages <- c("lidR", "sf", "rgdal", "raster", "terra")

# Install Packages if not Installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# Load Packages
invisible(suppressMessages(suppressWarnings(lapply(packages, library, character.only = TRUE))))

# Remove Variables
rm(installed_packages)
rm(packages)

# Set Raster Options
if(!dir.exists(path.expand("~/tmp"))) {
  dir.create(path.expand("~/tmp"))
}

clean_temp <- function() {
  files <- list.files(path = "~/tmp", full.names = TRUE)
  for (fn in files) {
    if (file.exists(fn)) {
      file.remove(fn)
    }
  }
}
clean_temp()

rasterOptions(tmptime = 72, tmpdir = path.expand("~/tmp"), progress = "")

# Retrieve Pulses Function
retrievePulses_ctg <- function(las){
  las <- readLAS(las) # read las
  if (is.empty(las)) return(NULL) # if chunk  isempty return null
  las <- retrieve_pulses(las) # retrieve pulses from las
  return(las) # return output
}

# Filter First Returns Function
filterFirst_ctg <- function(las){
  las <- readLAS(las) # read las
  if (is.empty(las)) return(NULL) # if chuck is empty return null
  las <- filter_first(las) # filter first returns on las
  return(las) # return output
}

# First Return Calculation Function
first_return_calc <- function(ReturnNumber, NumberOfReturns, cell_area) {
  filter <- ReturnNumber > 0 & NumberOfReturns > 0 & ReturnNumber <= NumberOfReturns
  ReturnNumber <- ReturnNumber[filter]
  NumberOfReturns <- NumberOfReturns[filter]
  
  first = sum(ReturnNumber == 1)
  first_den = sum(ReturnNumber == 1)/cell_area
  
  out <- list(First = first, FirstDen = first_den)
  
  return(out)
}

# First Return  Function
first_retrun <- function(ctg_folder, cell_size, outfolder){
  ctg <- readLAScatalog(ctg_folder)
  x <- grid_metrics(ctg, func = ~first_return_calc(ReturnNumber, NumberofReturns, (cell_size^2)), res = cell_size)
  writeRaster(x, file.path(outfolder, "first_return.tif"), format = "GTiff", overwrite = TRUE)
}

# Echo Metrics Calculation Function
echo_metrics_calc <- function(ReturnNumber, NumberOfReturns) {
  
  filter <- ReturnNumber > 0 & NumberOfReturns > 0 & ReturnNumber <= NumberOfReturns
  ReturnNumber <- ReturnNumber[filter]
  NumberOfReturns <- NumberOfReturns[filter]
  
  n = length(ReturnNumber)
  
  first = sum(ReturnNumber == 1)/n
  intermediate = sum(ReturnNumber > 1 & ReturnNumber < NumberOfReturns)/n
  last = sum(ReturnNumber == NumberOfReturns & ReturnNumber > 1)/n
  
  out <- list(PercFirst = first, PercIntermidiate = intermediate, PercLast = last)
  
  return(out)
}

# Echo Metrics Function
echo_metrics <- function(ctg_folder, cell_size, outfolder){
  ctg <- readLAScatalog(ctg_folder)
  x <- grid_metrics(ctg, func = ~echo_metrics_calc(ReturnNumber, NumberOfReturns), res = cell_size)
  writeRaster(x, file.path(outfolder, "echo_metrics.tif"), format = "GTiff", overwrite = TRUE)
}

# Generate DEM
generate_dem <- function(ctg_folder, cell_size, outfolder, outname = "elev.tif", filter = ""){
  ctg <- readLAScatalog(ctg_folder, filter = filter)
  dem <- grid_terrain(ctg, cell_size, tin())
  writeRaster(dem, file.path(outfolder, outname), format = "GTiff", overwrite = TRUE)
}

# Generate Footprint Function
generate_footprint <- function(ctg_folder, outfolder, outname = "footprint.shp", driver = "ESRI Shapefile"){
  ctg <- readLAScatalog(ctg_folder)
  footprint <- as.spatial(ctg)
  footprint <- st_as_sf(footprint)
  st_write(footprint, file.path(outfolder, outname), layer = "footprint", driver = driver)
  #writeOGR(footprint, file.path(outfolder, "footprint.shp"), layer = "footprint", driver = "ESRI Shapefile")
}

# Generate Boundaries Function
generate_boundaries <- function(ctg_folder, outfolder, concavity = 50, outname = "boundary.shp", driver = "ESRI Shapefile"){
  ctg <- readLAScatalog(ctg_folder)
  ctg <- catalog_boundaries(ctg, concavity = concavity, length_threshold = 15)
  boundary <- as.spatial(ctg)
  boundary <- st_as_sf(boundary)
  st_write(boundary, file.path(outfolder, outname), layer = "boundary", driver = driver)
}

# Generate Slope Function
generate_slope <- function(dem, outfolder, outname="slope.tif", unit="degrees") {
  print("Generate Slope")
  slope <- terrain(dem, opt="slope", unit=unit)
  writeRaster(slope, filename = file.path(outfolder, outname), format = "GTiff", overwrite = TRUE)
}

# Generate Contours Function
generate_contours <- function(dem, interval, outfolder){
  print("Generating Contours")
  dem <- raster(dem)
  n = ((max(pretty(dem[])) - min(pretty(dem[])))/interval)
  contours <- rasterToContour(dem, levels = pretty(range(dem[], na.rm = TRUE), n = n))
  writeOGR(contours, file.path(outfolder, "contours.shp"), layer = "contours", driver = "ESRI Shapefile")
}

# Generate Slope Classes Function
generate_slope_class <- function(slope, outfolder){
  slope <- raster(slope)
  max_deg <- max(slope[])
  m <- c(0, 27 , 1, 27, 34, 2, 34, 45, 3, 45, max_deg, 4)
  rclmat <- matrix(m, ncol = 3, byrow=TRUE)
  rc <- reclassify(slope, rclmat)
  writeRaster(rc, filename = file.path(outfolder, "slope_classified.tif"), format = "GTiff", overwrite = TRUE)
}

# Project LAS Function
project_las <- function(folderpath, epsg_code, outfolder, exportLAS = TRUE){
  laz_files <- list.files(folderpath, pattern = "*.laz$")
  for (file in laz_files){
    print(file)
    if (file.exists(file.path(outfolder, file))){
      print(paste(file, "exist"))
    } else{
      laz <- readLAS(file.path(folderpath, file))
      
      if (epsg(laz) == 0){
        epsg(laz) <- epsg_code
      } else{
        laz <- spTransform(laz, CRS(paste0("EPSG:", as.character(epsg_code))))
      }
      if (exportLAS == TRUE){
        writeLAS(laz, file.path(outfolder, file))
      }
    }
  }
  return(laz)
}

# Normalize LAZ Function
normalize_laz <- function(ctg_folder, dem, output) {
  ctg <- readLAScatalog(ctg_folder)
  dem <- raster(dem)
  if(!dir.exists(output)){
    dir.create(output)
  }
  opt_stop_early(ctg) <- FALSE
  opt_output_files(ctg) <-paste0(output, "/{ORIGINALFILENAME}")
  opt_laz_compression(ctg) <- TRUE
  normalize_height(ctg, dem)
}

# Generate Individual CHM's for Each Tile in Catalog
generate_ind_chm <- function(ctg, cell_size, outfolder){
  chm_ctg <- function(las){
    laz <- readLAS(las)
    if (is.empty(laz)) return(NULL)
    dem <- grid_terrain(laz, cell_size, tin())
    nlas <- normalize_height(laz, dem)
    nlas <- filter_poi(nlas, Z < 70 & Z > 2)
    chm <- grid_canopy(nlas, 1, pitfree(c(0,2,5,10,15), c(0,1.5)))
    return(chm)
  }
  opt_output_files(ctg) <- paste0(outfolder, "/chm_{ORIGINALFILENAME}")
  opt_stop_early(ctg) <- FALSE
  chm <- catalog_apply(ctg, chm_ctg)
  return(chm)
}

# Generate CHM Function
generate_chm <- function(ctg_normalized, cell_size, outfolder, outname = "chm.tif", filter = '-drop_z_below 2 -drop_z_above 65'){
  ctg <- readLAScatalog(ctg_normalized, filter = filter)
  chm <- grid_canopy(ctg, cell_size, pitfree(c(0,2,5,10,15), c(0,1.5)))
  writeRaster(chm, filename = file.path(outfolder, outname), format = "GTiff", overwrite = TRUE)
}


# Calculate Nominal Point Spacing
nps <- function(las){
  nps <- sqrt(1/density(las))
  return(nps)
}

# Calculate Minimum Cell Size
minCellSize <- function(las){
  minCellSize <- sqrt(sqrt(1/density(las)))
  return(minCellSize)
}

# Segment Snags Function
classify_snags <- function(ctg, outfolder){
  
  # Define bbpr thresholds
  bbpr_thresholds <- matrix(c(0.80, 0.80, 0.70,
                              0.85, 0.85, 0.60,
                              0.80, 0.80, 0.60,
                              0.90, 0.90, 0.55),
                            nrow = 3, ncol = 4)
  # Set Catalog Options
  opt_output_files(ctg) <- paste0(outfolder, "/{ORIGINALFILENAME}") # File Name
  opt_laz_compression(ctg) <- TRUE
  
  # Segment Snags
  ctg <- segment_snags(ctg, wing2015(neigh_radii = c(1.5, 1, 2), BBPRthrsh_mat = bbpr_thresholds))
  
  return(ctg)
}

# Find Snag Trees
snag_trees <- function(snag_ctg, outfolder=NULL, outfile = "snags_trees.gpkg", exportFile = TRUE){
  # Retrieve Snag Points Funciton
  retrieveSnags_ctg <- function(las){
    las <- readLAS(las) # Read Las
    if (is.empty(las)) return(NULL) # If Chunk is Empty Return NULL
    las <- filter_poi(las, snagCls > 0) # Filter Point Cloud to Points with snag classes
    ttops_snags <- locate_trees(las, lmf(5)) # Find Snagged Trees
    return(ttops_snags)
  }
  # Find Snagged Tree Tops
  ttops_snags <- catalog_apply(ctg, retrieveSnags_ctg)
  ttops_snags <- do.call(rbind, ttops_snags) # Add all sf features into one
  
  # Write to File
  if (exportFile == TRUE){
    st_write(ttops_snags, file.path(outfolder, outfile), layer = "snags", driver = "GPKG", delete_dsn = TRUE)
  }
  
  return(ttops_snags)
}

# Classify Ground 
ctg_classify_ground <- function(ctg_folder, outfolder){
  # Load Catalog and Set Parameters
  ctg <- readLAScatalog(ctg_folder)
  opt_output_files(ctg) <- paste0(outfolder, "/{ORIGINALFILENAME}")
  opt_laz_compression(ctg) <- TRUE
  
  # Cloth Simulation Filter
  mycsf <- csf(TRUE, 1, 1, time_step = 1)
  
  # Classify Ground
  classify_ground(ctg, mycsf)
}

# Las to Laz conversion
las_to_laz <- function(in_folder, out_folder){
  if(!dir.exists(out_folder)){
    dir.create(out_folder)
  }
  
  file_list <- list.files(path = in_folder, pattern = "*.las$")
  
  for (f in file_list){
    if (!file.exists(file.path(out_folder, gsub(".las", ".laz", f)))){
      print(f)
      tryCatch({
        las <- readLAS(file.path(in_folder, f))
        writeLAS(las, file.path(out_folder, gsub(".las", ".laz", f)))
      }, warning = function(w){
        print(w)
      }, error = function(e){
        print(e)
      })
    } else {
      print(paste0(f, " already exists"))
    }
    rm(las)
    gc()
  }
}

# Elevation Change Function
elev_change <- function(new_dem, old_dem, outfolder, outname = "elev_diff.tif"){
  if(!dir.exists(outfolder)){
    dir.create(outfolder)
  }
  new <- raster(new_dem)
  old <- raster(old_dem)
  dem_diff <- new - old_dem
  writeRaster(dem_diff, file.path(outfolder, outname))
}

# Find Tree Tops from Catalog
tree_tops <- function(ctg, outfolder=NULL, outfile = "ttops.gpkg", exportFile=TRUE){
  # Find Tree Tops from LAS
  find_ttops <- function(las){
    las <- readLAS(las)
    if (is.empty(las)) return(NULL)
    ttops <- locate_trees(las, lmf(5))
    return(ttops)
  }
  
  # Find Tree Tops from Catalog
  ttops <- catalog_apply(ctg, find_ttops) # Apply find_ttops function to catalog
  ttops <- do.call(rbind, ttops) # Bind all points objects into one
  
  # Export as GeoPackage
  if (exportFile == TRUE){
    st_write(ttops, file.path(outfolder, outfile), layer = "ttops", driver = "GPKG", delete_dsn = TRUE)
  }
  
  return(ttops)
}

# Merge sf objects
merge_sf <- function(inpath, outpath, outname, wildcard = "*.shp$"){
  feats <- list.files(inpath, wildcard, full.names = TRUE)
  feat <- st_read(feats[1])
  for(f in 2:length(feats)){
    f1 <- st_read(feats[f])
    feat <- rbind(feat, f1)
  }
  st_write(feat, file.path(outpath, outname))
}

# Denoise Catalog based off Liu et al. 2021 https://doi.org/10.1016/j.measurement.2021.109301
denoise_liu2021 <- function(ctg, outfolder, neighbours=10, multiplier=5){
  denoise_las <- function(las){
    las <- readLAS(las) # Read LAS
    if (is.empty(las)) return(NULL) # If chunk is empty return NULL
    print("Classify Noise")
    las <- classify_noise(las, sor(neighbours, multiplier, quantile = FALSE))
    print("Filter Noise")
    las <- filter_poi(las, Classification != LASNOISE)
    return(las)
  }
  
  if (!dir.exists(outfolder)){
    dir.create(outfolder)
  }
  
  opt_output_files(ctg) <- paste0(outfolder, "/{ORIGINALFILENAME}")
  opt_laz_compression(ctg) <- TRUE
  
  #
  denoise_ctg <- catalog_apply(ctg, denoise_las)
  return(denoise_ctg)
}

# Crown Metrics 
crown_metrics <- function(ctg, outpath, outname = "crowns.shp"){
  # Install EBImage Package if not already installed to run Watershed Segmentation
  ebimage_package <- "EBImage" %in% rownames(installed.packages())
  if (ebimage_package == FALSE){
    install.packages("BiocManager")
    BiocManager::install("EBImage")
  }
  rm(ebimage_package)
  
  # Segment Crowns Function
  seg_crowns <- function(las){
    las <- readLAS(las) # Read LAS
    if (is.empty(las)) return(NULL)
    chm <- rasterize_canopy(las, 0.5, pitfree(c(0,2,5,10,15), c(0,1.5))) # Create CHM
    trees <- segment_trees(las, watershed(chm))# Segment Trees
    crowns <- crown_metrics(trees, .stdtreemetrics, geom="concave", concaveman = c(3,0)) # Generate Crowns
    return(crowns)
  }
  opt_stop_early(ctg) <- FALSE # Errors in this function do not apply. This is a R Anomaly NOT an issue with function.
  crowns_ctg <- catalog_apply(ctg, seg_crowns)
  crowns_ctg <- do.call(rbind, crowns_ctg)
  st_write(crowns_ctg, file.path(outpath, outname))
  return(crowns_ctg)
}


# ROI Interesect
roi_intersect <- function(ctg, roi, filter=""){
  ctg <- readLAScatalog(ctg, filter = filter) # read in lidar files
  roi <- st_read(roi) # read in ROI file
  roi <- st_transform(roi, crs(ctg)) # transfrom crs of roi to match ctg
  ctg <- catalog_intersect(ctg, roi) # find intersecting tiles
  return(ctg) # return ctg
}
