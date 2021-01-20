# library(tidyverse)

library(raster)




Reading_tiff_file <- function(path_to_tiff , grid_points, Resolution = NULL) {
  
  tiff_file <- raster::raster(path_to_tiff)
  tiff_file_resolution <- raster::res(tiff_file)
  
  grid_points_extent <- raster::extent(min(grid_points$long), max(grid_points$long),
                                       min(grid_points$lat), max(grid_points$lat))
  
  #Subsetting raster file to the extent of grid points
  subsetted_raster <- raster::crop(tiff_file, grid_points_extent)
  
  #Estimating Resolution of grid points if not provided
  if(is.null(Resolution)) {
    Resolution_hat <- c(min(diff(sort(unique(grid_points$long)))), min(diff(sort(unique(grid_points$lat)))))
  }
  
  #Rescaling raster file to the provided resolution
  if(is.null(Resolution)) {
    
    aggregation_factor <- c(Resolution_hat[1] / tiff_file_resolution[1], Resolution_hat[2] / tiff_file_resolution[2])
    
  } else {
    
    aggregation_factor <- c(Resolution / tiff_file_resolution[1], Resolution / tiff_file_resolution[2])
  
  }
  
  rescaled_raster <- raster::aggregate(subsetted_raster, fact = aggregation_factor)
  
  
  #Extracting values from raster file
  suppressWarnings(Values <- raster::extract(rescaled_raster, grid_points[, c("long", "lat")]))
  
  #Building output
  output <- cbind(grid_points, values = Values)
  
  return(output)
}



tiff_path <- "roughness_1KMmn_GMTEDmd.tif"
grid_points <- read.table("fire.points.dat", header = T)


Reading_tiff_file(tiff_path, grid_points)
