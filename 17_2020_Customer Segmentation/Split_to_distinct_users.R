grouped_data <- read_csv("group179000.csv", n_max = 1000000, col_types = "cDdcddd")

length(unique(grouped_data$V1))



users <- unique(grouped_data$V1)
users <- tibble(V1 = users,
                split = sort(rank(users)%%9))

grouped_data <- left_join(grouped_data, users, by = "V1") %>% 
  nest_by(split)

dir.create("data_distinct_users")
map2(grouped_data$data, paste0("data_distinct_users/grouped_data_", 1:9, ".csv"), ~ write_csv(.x, path = .y))

dir.create("features_distinct_users")

for(i in 1:9) {
  grouped_data <- read.csv(paste0("data_distinct_users/grouped_data_", i, ".csv"))
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
                         Weekend = day %in% 6:7,
                         Weekday = day %in% 1:5,
                         Daytime = V24 %in% 9:15,
                         Evening = V24 %in% 16:22) %>% 
    filter(V6 == "VOICE") %>% 
    group_by(V1) %>% 
    summarise(avg_call_duration = mean(sum_duration),
              percent_weekend = mean(Weekend),
              percent_weekday = mean(Weekday),
              percent_daytime = mean(Daytime),
              percent_evening = mean(Evening), .groups = "drop")
  
  
  Features <- left_join(avg_per_day, percent_time, by = "V1") %>% 
    mutate(avg_call_duration = ifelse(avg_calls_day == 0, 0, avg_call_duration),
           across(starts_with("percent"), ~ifelse(avg_calls_day == 0, 0, .)))
  
  write_csv(avg_per_day, path = paste0("features_distinct_users/Features_", i, ".csv"))
}


foreach(i = 1:9) %do% {
  grouped_data <- read.csv(paste0("data_distinct_users/grouped_data_", i, ".csv"))
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
                         Weekend = day %in% 6:7,
                         Weekday = day %in% 1:5,
                         Daytime = V24 %in% 9:15,
                         Evening = V24 %in% 16:22) %>% 
    filter(V6 == "VOICE") %>% 
    group_by(V1) %>% 
    summarise(avg_call_duration = mean(sum_duration),
              percent_weekend = mean(Weekend),
              percent_weekday = mean(Weekday),
              percent_daytime = mean(Daytime),
              percent_evening = mean(Evening), .groups = "drop")
  
  write_csv(avg_per_day, path = paste0("features_distinct_users/Avg_activity_per_day_", i, ".csv"))
  write_csv(percent_time, path = paste0("features_distinct_users/percent_per_time_", i, ".csv"))
  
}