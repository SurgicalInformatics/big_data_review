library(tidyverse)


wb = read.csv("world_bank_class.csv")
names(wb) = tolower(names(wb))
library(dplyr)
library(magrittr)
wb %>% 
	filter(groupname == "Low & middle income") %$% 
	paste0(countryname, collapse = "|")

wb_lmic = wb %>% 
	filter(groupname == "Low & middle income" | groupname  == "High income") %>% 
	select(country = countryname, hic_or_lmic = groupname) %>% 
	mutate(hic_or_lmic = fct_recode(hic_or_lmic, "lmic" = "Low & middle income", "hic" = "High income"))


gs_countries = read_csv("countries_gs1_gs2.csv")

gs1  = gs_countries %>% 
	filter(study_gs == 1) %>% 
	select(gs1 = hdi_countries) %>% 
	arrange(gs1)

gs2  = gs_countries %>% 
	filter(study_gs == 2) %>% 
	select(gs2 = hdi_countries) %>% 
	arrange(gs2)


write_csv(gs1, path = "gs1_countries.csv")
write_csv(gs2, path = "gs2_countries.csv")
write_csv(wb_lmic, path = "wb_lmic_countries.csv")






