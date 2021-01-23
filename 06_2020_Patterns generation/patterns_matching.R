t0 = Sys.time()
#install required packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("purrr")) install.packages("purrr")
if (!require("rjson")) install.packages("rjson")

#loading required packages
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(rjson)

#read in domains file
t0 = Sys.time()
domains_names <- read_delim("net.txt", col_types = "c",
                            delim = "\t", skip = 1000000, n_max = 10000000)
  
domains_names <- domains_names[[1]] %>% 
  # str_subset(".net.$") %>%
  str_remove(".net.$")

#matching patterns with domains names
unzip("patterns.zip")
patterns_combinations <- map(list.files(pattern = "*.csv"), read.csv, colClasses = "character") %>% map(1)

not_in <- function(comb, data)
  return(subset(comb, !(comb %in% data)))

comb_in <- map(patterns_combinations, not_in, rep(domains_names, 1000))
Sys.time() - t0

#Write json
names(comb_in) = list.files(pattern = "*.csv") %>% 
  str_remove(".csv$")

write(toJSON(comb_in), "output.json")
Sys.time() - t0