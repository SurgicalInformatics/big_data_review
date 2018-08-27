


wb = read.csv("world_bank_class.csv")
names(wb) = tolower(names(wb))
library(dplyr)
library(magrittr)
wb %>% 
	filter(groupname == "Low & middle income") %$% 
	paste0(countryname, collapse = "|")

