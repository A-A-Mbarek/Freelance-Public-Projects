library(tidyverse)
# library(tiff)
library(raster)
# library(rgdal)

# memory.limit(size=10000000)
elevation_file <- raster::raster("elevation_1KMmd_GMTEDmd.tif", resolution = c(1,1))
fire_data <- read.table("fire.points.dat", header = T)

coords_limit <- list(x = c(min = min(fire_data$long), max = max(fire_data$long)),
                     y = c(min = min(fire_data$lat), max = max(fire_data$lat)))

Resolution <- res(elevation_file)

trunc_coords <- expand.grid(x = seq(coords_limit$x["min"], coords_limit$x["max"], by = Resolution[1]),
                            y = seq(coords_limit$y["min"], coords_limit$y["max"], by = Resolution[2]))



# x_ind_min <- which(seq(-180, 180, by = Resolution[1]) > coords_limit$x[1])[1]
# x_ind_max <- which(seq(-180, 180, by = Resolution[1]) > coords_limit$x[2])[1]
# y_ind_min <- which(seq(-56, 84, by = Resolution[2]) > coords_limit$y[1])[1]
# y_ind_max <- which(seq(-56, 84, by = Resolution[2]) > coords_limit$y[2])[1]

e <- extent(coords_limit$x[1], coords_limit$x[2], coords_limit$y[1], coords_limit$y[2])
# subsetted_raster <- crop(elevation_file, extent(elevation_file, x_ind_min, x_ind_max, y_ind_min, y_ind_max))
subsetted_raster_e <- crop(elevation_file, e)



str(elevation_file)
summary(elevation_file)
plot(elevation_file)

v <- getValues(elevation_file)
length(v)

xy <- cbind(-50, seq(-80, 80, by=20))

# subsetted_raster <- raster::extract(elevation_file, trunc_coords)

RES0 = 0.025
rescaled_elevation <- aggregate(subsetted_raster_e, fact = 0.25 / Resolution[1])

all(unique(fire_data$lat) %in% seq(rescaled_elevation@extent@ymin, rescaled_elevation@extent@ymax, by = res(rescaled_elevation)[2]))
all(unique(fire_data$long) %in% seq(rescaled_elevation@extent@xmin, rescaled_elevation@extent@xmax, by = res(rescaled_elevation)[1]))

#next step raster::extract