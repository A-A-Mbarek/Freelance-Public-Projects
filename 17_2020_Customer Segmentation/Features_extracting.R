#install tidyverse packages in not already installed
if (!require("tidyverse")) install.packages("tidyverse")

library(tidyverse)

# set the current file location as the default working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# check the working directory
getwd()

#Read in the "fileList.txt" file
files_names <- read.table(file.choose())[,1]

grouped_data_split <- files_names %>% 
  purrr::map_df(readr::read_csv, col_types = "cDdcddd")

K = 9 #number of splits
users <- unique(grouped_data_split$V1)
users <- tibble(V1 = users,
                split = sort(rank(users)%%K))

grouped_data_split <- left_join(grouped_data_split, users, by = "V1") %>% 
  nest_by(split)

# dir.create("data_distinct_users")
# map2(grouped_data$data, paste0("data_distinct_users/grouped_data_", 1:K, ".csv"), ~ write_csv(.x, path = .y))

if(!dir.exists("features_distinct_users"))
  dir.create("features_distinct_users")

for(i in 1:K) {
  grouped_data <- grouped_data_split$data[[i]]
  grouped_data_split$data[[i]] <- data.frame()
  #Average number of calls/SMS per day
  
  avg_per_day <- mutate(grouped_data, date = lubridate::ymd(V23)) %>% 
    group_by(V1, date, V6) %>% 
    summarise(number = n(), .groups = "drop") %>% 
    group_by(V1, V6) %>% 
    summarise(avg_num_day = mean(number), .groups = "drop") %>% 
    pivot_wider(names_from = V6, values_from = "avg_num_day", values_fill = 0) %>% 
    rename(avg_sms_day = SMS, avg_calls_day = VOICE)
  
  #Average call duration & Percentage calls/time
  
  percent_time <- mutate(grouped_data, date = lubridate::ymd(V23),
                         day = lubridate::wday(date),
                         Friday = day == 6,
                         Saturday = day == 7,
                         Weekday = day %in% 1:5,
                         Daytime = V24 %in% 9:15,
                         Evening = V24 %in% 16:22) %>% 
    filter(V6 == "VOICE") %>% 
    group_by(V1) %>% 
    summarise(avg_call_duration = mean(sum_duration),
              percent_friday = mean(Friday),
              percent_saturday = mean(Saturday),
              percent_weekday = mean(Weekday),
              percent_daytime = mean(Daytime),
              percent_evening = mean(Evening), .groups = "drop")
  
  Features <- left_join(avg_per_day, percent_time, by = "V1") %>% 
    mutate(avg_call_duration = ifelse(avg_calls_day == 0, 0, avg_call_duration),
           across(starts_with("percent"), ~ifelse(avg_calls_day == 0, 0, .)))
  
  write_csv(Features, path = paste0("features_distinct_users/Features_", i, ".csv"))
  
}

