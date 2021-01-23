library(tidyverse)
library(lctools)

data(GR.Municipalities)
data <- GR.Municipalities@data


#Dispersion Diagram
Z <- data$Income01 - mean(data$Income01)

W_matrix <- w.matrix(Coords = cbind(data$X, data$Y), Bandwidth = 6)

WZ <- t(t(W_matrix) * Z)
WZ_sums <- apply(WZ, 1, sum) / 6

data_plot <- data.frame(z = Z, wzs = WZ_sums)

ggplot(data_plot, aes(z, wzs))+
  geom_point(alpha = 0.5)+
  xlim(c(-10000, 15000))+
  ylim(c(-5000, 10000))+
  theme_minimal()+
  labs(title = "Dispersion Diagram", x = "normalized income",
       y = "normalized weighted sums of the incomes")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_abline(slope = 1, intercept = 0, color = "red", alpha = 0.7, linetype = "longdash")+
  geom_text(label = "y = x", x = 10000, y = 10000, color = "red", alpha = 0.7, angle = 45)


##Moran's I

I <- moransI(Coords = cbind(data$X, data$Y), Bandwidth = 6, x = data$UnemrT01)

M_I_k <- function(k) {
  
  result <- moransI(Coords = cbind(data$X, data$Y), Bandwidth = k, x = data$UnemrT01)
  
  return(unlist(result[-1]))
}

k = c(3, 5, 9, 12, 15, 18, 20, 24, 30)

map(k, M_I_k) %>% 
  do.call(rbind,.) %>% 
  data.frame(k, .)

#Histograms

Unemp_data <- data.frame(Gender = "Men", UnempR = data$UnemrM01) %>% 
  rbind(., data.frame(Gender = "Women", UnempR = data$UnemrF01))

ggplot(Unemp_data, aes(x = UnempR, fill = Gender, color = Gender))+
  geom_histogram(color = "black")+
  facet_wrap(~Gender)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 35, 5))+
  labs(title = "Histogram of Unemployment rate of men and women",
       x = "", y = "Frequence")


#Boxplots

ggplot(Unemp_data, aes(x = UnempR, y = Gender, fill = Gender))+
  geom_boxplot()+
  theme_minimal()+
  guides(fill = F)+
  scale_x_continuous(breaks = seq(0, 32, 2))+
  labs(title = "Boxplot of Unemployment rate of men and women",
       y = "", x = "Unemployment Rate")