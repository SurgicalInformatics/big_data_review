library(tidyverse)

originaldata = read_csv("final_included_papers_august2018.csv", skip = 19) %>% 
	mutate_if(is.character, str_trim) %>% 
	mutate(study_id = paste(author %>% tolower %>% str_replace_all(" ", "_"), year, sep = "-")) %>% 
	mutate(study_design = fct_recode(study_design,
																	 "Cohort (single centre)" = "Cohort (one centre)",
																	 "Cohort (country)"       = "Cohort (single country)") %>% 
				 	fct_relevel("Cohort (single centre)", "Cohort (country)", "Cohort (multiple countries)", "Database")) %>% 
	mutate(surgical_speciality = fct_recode(surgical_speciality,
																				 "Breast surgery"        = "Breast",
																				 "General surgery"       = "General",
																				 "General surgery"       = "General Surgery",
																				 "ENT"                   = "Head and neck",
																				 "Multiple specialities" = "Multiple",
																				 "Multiple specialities" = "Multiple specialities (elective surgery)"))


# seperate studies with multiple countries where n per country not available:
multi_unseparated = originaldata %>% 
	filter(! (n_comment == "multiple" | is.na(n_comment)) )

single_or_separated = originaldata %>% 
	filter( (n_comment == "multiple" | is.na(n_comment)) )

if (nrow(multi_unseparated) + nrow(single_or_separated) != nrow(originaldata)){
	break("Rows don't add up, something missing")
}else{
	print("ALL GOOD: Studies without n patients per country separated.")
}

mydata =  single_or_separated %>% 
	select(study_id, year, n_studytotal = n, countries, surgical_speciality, study_design) %>% 
	separate(countries, into = paste0("country_", 1:15), sep = ",") %>% 
	gather(country_n, country, -study_id, -year, -n_studytotal, -surgical_speciality, -study_design) %>% 
	drop_na(country) %>% 
	select(-country_n) %>% 
	mutate(n_country = str_extract(country, "\\d+") %>% as.numeric()) %>% #extract number (in brackets after the country name), e.g. India (1000)
	mutate(country   = str_remove(country, "\\(\\d+\\)") %>% str_trim()) %>% # now remove the e.g. (1000) - digit in brackets
  mutate(n_country = ifelse(is.na(n_country), n_studytotal, n_country)) %>% 
	ungroup()

mydata %>% 
	ggplot(aes(x = year)) + 
	geom_bar()

# check for duplicate country name spellings
# clean_names = mydata %>% 
# 	distinct(country, .keep_all = TRUE) %>% 
# 	arrange(country)

country_summary = mydata %>% 
	group_by(country) %>% 
	mutate(n_studies = n()) %>% 
	mutate(n_patientstotal = sum(n_country)) %>% 
	ungroup()

save(country_summary, file = "02_country_summary.rda")
save(mydata,          file = "02_alldata.rda")




