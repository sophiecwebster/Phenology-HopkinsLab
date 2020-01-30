require(phenocamapi)
require(rgdal)
require(sp)
library(tidyverse)
library(dplyr)
library(phenocamapi)
library(jpeg)
library(data.table)
require(daymetr)
require(fields)
require(raster)

# setting up phenocam data

# import pheno metadata as table
meta <- get_phenos()

# make lowercase
meta$dominant_species <- tolower(meta$dominant_species)

# find rows of sites where predominant species is acer rubrum
acer.sites <- grep("acer rubrum", meta$dominant_species)

# find rows of sites where predominant species is quercus rubra
queru.sites <- grep("quercus rubra", meta$dominant_species)
queru.sites <- c(queru.sites, grep("quercus sp", meta$dominant_species))
both.sites <- intersect(acer.sites, queru.sites)

# add column for vegetation type (either acer, quercus, or both)
meta$Veg <- "ACRU"
meta$Veg[queru.sites] <- "QURU"
meta$Veg[both.sites] <- "BOTH"

# choose sites of interest, make into data table
choose.sites <- c(acer.sites,queru.sites)
choose.sites <- as.numeric(sort(choose.sites))
choose.sites <- unique(choose.sites)
all.sites <- as.data.frame(meta)[choose.sites,]

# find sites with corresponding flux tower data
ttower <- group_by(all.sites, flux_data = TRUE)
quru <- group_by(ttower, Veg == "QURU")

view(ttower)

# 36 sites are ACRU only, 16 are QURU only, 15 are both
# save out data

#sites    <- ttower[which(ttower$Veg == "QURU" | ttower$Veg == "BOTH"),]
#site_map <- sites
#coordinates(site_map) = ~lon+lat
#proj4string(site_map) <- CRS("+proj=longlat +datum=WGS84")

#########################
# Extracting Climate Data
#########################

# using only sites with co-located flux tower; pulling site name, lat, lon 
# (all.sites is used in fig 1)

sites <- all.sites
latlon <- sites[c("lat", "lon", "site")]

# Pull Climate Data

# loop through sites and gather data
climate_results <- c()
for(j in 1:nrow(latlon)) {
  
  print(j)

  # Variables
  years <- 1980:2017
  Latitude <- latlon[j, "lat"]
  Longitude <- latlon[j, "lon"]
  
  #Download climate data
  climate <- download_daymet(site = "Oak Ridge National Laboratories",
                             lat = Latitude,
                             lon = Longitude,
                             start = years[1],
                             end = years[length(years)],
                             internal = TRUE, force = T, silent = T)
  climate.data <- climate$data

}