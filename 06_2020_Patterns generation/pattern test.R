library(tidyverse)
#CVCV pattern
Vp = "[aeiou]"
Cp = "[b-df-hj-np-tv-z]"
pattern_test = paste0('\\b(', Cp, Vp, '){2}')

#NNNN pattern
pattern_test = '^[0-9]{4}$'
domains_names <- read.delim("net-min-1k.txt", fileEncoding="UTF-16LE",
                            sep = "\t", skip = 1)

domains_names <- str_subset(domains_names[,1], ".net.$") %>% 
  str_remove(".net.$")

#domains names which do not match 'NNNN' pattern 
non_matches <- subset(domains_names, !str_detect(domains_names, pattern_test))
write_lines(non_matches, path = "NNNN.txt")


######Combinations not in the file
unzip("patterns.zip")
patterns_combinations <- map(list.files(pattern = "*.csv"), read.csv, colClasses = "character") %>% map(1)
LLLLL_all_combinations <- read.csv("LLLLL.csv", colClasses = "character")[,1]

not_in <- function(comb, data)
  return(subset(comb, !(comb %in% data)))

t0 = Sys.time()
comb_in <- map(patterns_combinations, not_in, domains_names)
Sys.time() - t0

map(comb_in, writexl::write_xlsx, )
write_lines(NNNN_not_in, path = "NNNN_not_in_file.txt")



#Write json
names(comb_in) = list.files(pattern = "*.csv") %>% 
  str_remove(".csv$")

# install.packages("rjson")
library(rjson)
write(toJSON(comb_in), "output.json")