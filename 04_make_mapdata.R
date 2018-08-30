library(tidyverse)
library(ggmap)
library(maptools)

load("02_country_summary.rda")

lmic_countries = read_csv("world_bank_class.csv") %>% 
	filter(GroupName == "Low & middle income")

country_summary = country_summary %>% 
	mutate(country = fct_recode(country,
															"Lao People's Democratic Republic" = "Lao PDR",
															"Burma" = "Myanmar",
															"Viet Nam" = "Vietnam",
															"Iran (Islamic Republic of)" = "Iran"
	) %>% as.character())

# Get map ------------
# https://groups.google.com/forum/#!msg/ggplot2/KAKhoE0GO4U/lsAXwQWNwhoJ
# Sundar Dorai-Raj
PolygonCoords <- function(polygon) {
	polygons <- polygon@Polygons
	coords.list <- lapply(seq_along(polygons), function(i) {
		# Extract the group, sequence, area, longitude, and latitude.
		coords <- polygons[[i]]@coords
		cbind(i, 1:nrow(coords), polygons[[i]]@area, coords)
	})
	coords.df <- as.data.frame(do.call(rbind, coords.list))
	names(coords.df) <- c("order", "seq", "area", "long", "lat")
	return(coords.df)
}

ConvertWorldSimple <- function(mapdata, min.area = 0) {
	# min.area is the minimum size of the polygons. Setting to some  # positive value will filter out tiny islands.
	coords.list <- lapply(mapdata@polygons, PolygonCoords)
	ncoords <- sapply(coords.list, nrow)
	coords.df <- do.call(rbind, coords.list)
	coords.df$country <- rep(mapdata@data$NAME, ncoords)
	country.group <- factor(paste(coords.df$country, coords.df$order))
	coords.df$group <- as.numeric(country.group)
	coords.df <- coords.df[coords.df$area >= min.area, ]
	return(coords.df)
}


data("wrld_simpl")
#remove small islands
world_map <- ConvertWorldSimple(wrld_simpl, min.area = 0.8) %>% 
	filter(country != "Antarctica")


# check that names match
countries_matching = world_map %>% 
	distinct(country) %>% 
	#rename(country = region) %>% 
	mutate(nameorigin = "mapdata")

mismatches = country_summary %>% 
	left_join(countries_matching) %>% 
	filter(is.na(nameorigin))

mapdata = left_join(world_map, country_summary)

save(mapdata, file = "04_myworldmap.rda")

