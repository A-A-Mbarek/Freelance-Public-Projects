

extract_from_netcdf <- function(netCDF_file, coord_file, weekly_output = T) {
  
  dimensions_names <- names(netCDF_file$dim)
  dimensions <- purrr::map(dimensions_names, ~ ncdf4::ncvar_get(netCDF_file, .))
  
  variables <- names(netCDF_file$var)
  records_array <- ncdf4::ncvar_get(netCDF_file, variables)
  
  
  ##Recoding NAs
  fillvalue <- ncdf4::ncatt_get(netCDF_file,variables,"_FillValue")
  
  #Getting the time dimension
  time_origin <- ncdf4::ncatt_get(netCDF_file, dimensions_names[1],"units")$value %>% 
    strsplit(" ") %>% unlist() %>% .[3] %>% 
    strsplit("-") %>% unlist() %>% as.numeric()
  
  time_dimension <- chron::chron(dimensions[[1]], origin = time_origin[c(2,3,1)]) %>% 
    lubridate::mdy()
  
  nearest_points_data <- coord_file %>% as_tibble() %>% 
    distinct(lat, long) %>% 
    mutate(across(c("lat", "long"), ~round(.*4)/4, .names = "nearest_{col}")) %>% 
    mutate(nearest_long = ifelse(nearest_long > max(dimensions[[2]]), max(dimensions[[2]]), nearest_long),
           nearest_lat = ifelse(nearest_lat > max(dimensions[[3]]), max(dimensions[[3]]), nearest_lat)) %>% 
    rowwise() %>% 
    mutate(dim_ind_lat = which(dimensions[[3]] == nearest_lat),
           dim_ind_long = which(dimensions[[2]] == nearest_long)) %>% 
    ungroup()
  

  all_rows_bined <- coord_file %>% 
    distinct(id, lat, long) %>% 
    left_join(nearest_points_data) %>% as_tibble() %>% 
    mutate(records = map2(dim_ind_long, dim_ind_lat,
                                     ~ records_array[.x, .y, ]),
           Date = list(time_dimension)) %>% 
    select(id, lat, long, Date, records) %>% 
    unnest(cols = c("records", "Date")) %>% 
    mutate(records = ifelse(records == fillvalue$value, NA, records)) %>% 
    mutate(Month = lubridate::month(Date), .before = Date)
  
  
  summary_by_month <- group_by(all_rows_bined, id, Month) %>% 
    summarise(mean = mean(records, na.rm = T),
              max = max(records, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(id, Month) %>% 
    mutate(year = time_origin[1], .before = Month)
  
  
  summary_by_week <- mutate(all_rows_bined, Week = (lubridate::day(Date)  %/% 7) + 1) %>% 
    filter(Week < 5) %>% 
    group_by(id, Month, Week) %>% 
    summarise(mean = mean(records, na.rm = T),
              max = max(records, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(id, Month, Week) %>% 
    mutate(year = time_origin[1], .before = Month)
  
  
  event_specific_data <- summary_by_week %>% 
    semi_join(filter(coord_file, year == time_origin[1]),
              by = c("id", "year", "Month" = "month")) %>% 
    group_by(id, Month) %>% 
    summarise(MeanClimWeek.event = mean(mean, na.rm = T),
              MaxClimWeek.event = max(max, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(summary_by_month) %>% 
    rename(MeanClimMonth.event = mean, MaxClimMonth.event = max) %>% 
    relocate(year, .before = Month)
  
  
  if(weekly_output)
    return(list(year = time_origin[1],
                monthly_means = select(summary_by_month, -max),
                weekly_means = select(summary_by_week, -max),
                event_specific_data = event_specific_data))
  else
    return(list(year = time_origin[1],
                monthly_means = select(summary_by_month, -max),
                event_specific_data = event_specific_data))
  
}


library(tidyverse)
netCDF_file <- ncdf4::nc_open("FWI_m_2010.nc")
coord_file <- read.delim("GFED4_all_years_50lat_subset.dat", header = TRUE, sep=" ")


with_weekly_output <- extract_from_netcdf(netCDF_file, coord_file)

without_weekly_output <- extract_from_netcdf(netCDF_file, coord_file, weekly_output = F)

netCDF_files_list <- list.files(pattern = "^FWI") %>% 
  map(ncdf4::nc_open)

library(foreach)

results_list <- foreach(netCDF = netCDF_files_list) %do% extract_from_netcdf(netCDF, coord_file)

map(results_list, "year")

Monthly_means_all_years <- map(results_list, "monthly_means") %>% 
  bind_rows()

Weekly_means_all_years <- map(results_list, "weekly_means") %>% 
  bind_rows()

