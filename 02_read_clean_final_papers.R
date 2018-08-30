library(tidyverse)

allstudies = read_csv("final_included_papers_august2018.csv", skip = 19) %>% 
	mutate_if(is.character, str_trim) %>% 
	mutate(study_id = paste(author %>% tolower %>% str_replace_all(" ", "_"), year, sep = "-")) %>% 
	mutate(study_design = fct_recode(study_design,
																	 "Cohort (single centre)" = "Cohort (one centre)",
																	 "Cohort (country)"       = "Cohort (single country)") %>% 
				 	fct_relevel("Cohort (single centre)", "Cohort (country)", "Cohort (multiple countries)", "Database")) %>% 
	mutate(surgical_speciality = fct_recode(surgical_speciality,
																				 "Breast surgery"        = "Breast",
																				 "General surgery"       = "General",
																				 "General surgery"       = "Urology",
																				 "General surgery"       = "General Surgery",
																				 "ENT"                   = "Head and neck",
																				 "Multiple specialities" = "Multiple",
																				 "Multiple specialities" = "Multiple specialities (elective surgery)")) %>% 
	mutate(surgical_speciality = fct_collapse(surgical_speciality,
																						"ENT/OMS"                    = c("ENT", "Maxillofacial surgery"),
																						"Obstetrics and gynaecology" = c("Obstetrics and gynaecology", "Gynaecology"))) %>% 
	mutate(outcome_primary = fct_recode(outcome_primary, "Patient characteristics" = "Patient demographics",
																			"Outcomes of surgery" = "Post-operative outcomes") %>% 
				 	fct_relevel("Patient characteristics", "Incidence", "Access", "Outcomes of surgery", "Patient cost", "Survival")) %>% 
	mutate(n_comment = fct_explicit_na(n_comment, "single"))
	

allstudies$outcome_primary %>% unique()


# seperate studies with multiple countries where n per country not available:
multi_unseparated = allstudies %>% 
	filter(n_comment == "multiple_worldwide" | n_comment == "multiple_hic")

single_or_separated = allstudies %>% 
	filter((! (n_comment == "multiple_worldwide" | n_comment == "multiple_hic")) | is.na(n_comment))

if (nrow(multi_unseparated) + nrow(single_or_separated) != nrow(allstudies)){
	stop("Rows don't add up, something missing or duplicated")
}else{
	print("ALL GOOD: Studies without n patients per country separated.")
}

single_or_separated$n %>% sum()
single_or_separated$n %>% range()

single_or_separated %>% 
	count(n>3000)

country_data =  single_or_separated %>% 
	select(study_id, year, n_studytotal = n, countries, surgical_speciality, study_design) %>% 
	separate(countries, into = paste0("country_", 1:30), sep = ",") %>% 
	gather(country_n, country, -study_id, -year, -n_studytotal, -surgical_speciality, -study_design) %>% 
	drop_na(country) %>% 
	select(-country_n) %>% 
	mutate(n_country = str_extract(country, "\\d+") %>% as.numeric()) %>% #extract number (in brackets after the country name), e.g. India (1000)
	mutate(country   = str_remove(country, "\\(\\d+\\)") %>% str_trim()) %>% # now remove the e.g. (1000) - digit in brackets
  mutate(n_country = ifelse(is.na(n_country), n_studytotal, n_country)) %>% 
	ungroup()

allstudies %>% 
	ggplot(aes(x = fct_infreq(surgical_speciality))) + 
	geom_bar() +
	coord_flip() +
	theme_bw()

# check for duplicate country name spellings
# clean_names = mydata %>% 
# 	distinct(country, .keep_all = TRUE) %>% 
# 	arrange(country)

country_summary = country_data %>% 
	group_by(country) %>% 
	mutate(n_studies = n()) %>% 
	mutate(n_patientstotal = sum(n_country)) %>% 
	ungroup()

save(country_summary, file = "02_country_summary.rda")
save(country_data,    file = "02_country_data.rda")
save(allstudies,      file = "02_allstudies.rda" )




