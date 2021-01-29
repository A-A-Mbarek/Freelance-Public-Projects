#install tidyverse packages in not already installed
if (!require("tidyverse")) install.packages("tidyverse")

library(tidyverse)

# set the current file location as the default working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# check the working directory
getwd()

files_names <- list.files("features_distinct_users/") %>% 
  paste0("features_distinct_users/",.)
Features <- files_names %>% 
  purrr::map_df(readr::read_csv, col_types = "cdddddddd") %>% 
  na.omit()

#Condition of distinct users per file
length(unique(Features$V1)) == length(Features$V1)
  
#Preprocessing
data_m <- scale(Features[-1])

#Run Kmeans with 8 clusters, change it if you want
k_means_model <- kmeans(data_m, centers = 8)

Features_clusters <- mutate(Features, cluster = k_means_model$cluster)

write.csv(Features_clusters, "Clustering_results.csv")  

Clusters_summary <- Features_clusters %>%   
  group_by(cluster) %>% 
  summarise(across(everything(),
                   list(min = min, max = max, mean = mean, median = median),
                   .names = "{col}-{fn}")) %>% 
  pivot_longer(cols = -cluster) %>% 
  separate(name, into = c("feature", "statistic"), sep = "-") %>% 
  pivot_wider(names_from = statistic, values_from = value) %>% 
  select(feature, cluster, min, max, mean, median) %>% 
  arrange(feature, cluster)

write.csv(Clusters_summary, "clustering_summary.csv")

#If you have time excute this snippet to run the kmeans 13 times and send me the generated plot
max_k = 15

Kmeans_3_15 <- tibble(k = 3:max_k,
                      total_within_ss = map_dbl(3:max_k, ~kmeans(data_m, centers = .)$tot.withinss))
write.csv(Kmeans_3_15, "Kmeans.csv")

Kmeans_3_15 %>% 
  ggplot(aes(x = k, y = total_within_ss))+
  geom_line()+
  scale_x_continuous(breaks = 1:max_k)+
  theme_bw()  



