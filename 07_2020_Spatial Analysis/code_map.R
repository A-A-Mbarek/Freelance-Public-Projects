#!Mapping the municipalities
#!and color according to unemployment rate level

library(tidyverse)
library(lctools)
data(GR.Municipalities)
data_by_Municipality <- GR.Municipalities@data

# Clustering UnempR -------------------------------------------------------
k = 6
data_by_Municipality$UnemrT_cluster <- cut(data_by_Municipality$UnemrT01, breaks = k)
classes_bandwidth <- group_by(data_by_Municipality, UnemrT_cluster) %>% 
    summarise(min_ban = round(min(UnemrT01)),
              max_ban = round(max(UnemrT01)))

classes_bandwidth <- c(classes_bandwidth$min_ban, classes_bandwidth$max_ban[k])

class_labels <- paste0("[", classes_bandwidth, ",", lead(classes_bandwidth), "[")
class_labels <- class_labels[-(k+1)]

cluster_fun <- function(x, Bands, Labels) {
  k = length(Bands)
  for (i in 2:k) {
    if (x >= Bands[i-1] & x < Bands[i])
      return(Labels[i-1])
  }
}

data_by_Municipality <- rowwise(data_by_Municipality) %>% 
  mutate(class_unemr = cluster_fun(UnemrT01, classes_bandwidth, class_labels),
         class_unemr = factor(class_unemr, levels = class_labels))
                            

# Descriptives for UnempRate ----------------------------------------------
Custom_theme <- theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "None")

ggplot(data_by_Municipality, aes(x = class_unemr, fill = class_unemr))+
  geom_bar()+
  Custom_theme+
  scale_fill_brewer(palette = "Greens")+
  labs(x = "Classes of Unemployment Rate",
       y = "Number of Municipalities")
  


ggplot(data_by_Municipality, aes(fill = class_unemr))+
  geom_boxplot(aes(y = UnemrT01))+
  scale_y_continuous(breaks = 0:20)+
  facet_grid(~class_unemr, scale = "free")+
  Custom_theme +
  scale_fill_brewer(palette = "Greens")+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())+
  labs(y = "Unemployment Rate")
  
  

# Data for mapping --------------------------------------------------------
map_data <- fortify(GR.Municipalities, region = "OBJECTID") %>% 
  mutate(id = as.numeric(id)) %>%
  left_join(data_by_Municipality, by = c("id" = "OBJECTID"))



# Mapping -----------------------------------------------------------------
library(RColorBrewer)
map_data %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "gray80", aes(fill = class_unemr)) +
  scale_fill_brewer(palette = "Greens") +
  coord_equal() +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(x= "", y= "", fill= "Class of unemployment rate")

