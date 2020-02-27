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
  years <- 1980:2018
  Latitude <- latlon[j, "lat"]
  Longitude <- latlon[j, "lon"]
  print(Latitude)
   
   #Download climate data
   climate <- download_daymet(site = "Oak Ridge National Laboratories",
                              lat = Latitude,
                              lon = Longitude,
                              start = years[1],
                              end = years[length(years)],
                              internal = TRUE, force = T, silent = T)
  
   climate.data <- climate$data
   
   #Pull out month and day info
   
   date.list <- c()
   for(year in 1:length(years)){
     DOY <- climate.data$yday[climate.data$year == years[year]] #<- what is "years" doing here?
     date <- as.Date(DOY, origin = paste0(years[year], "-01-01"))
     date.list <- c(date.list, as.character(date))
   }
   date.list <- as.Date(date.list)
   month <- as.numeric(format(date.list, "%m"))
   climate.data$month <- month
   
   ### Climate Variables
   
   # Mean Temp (each day)
   climate.data$tmean <- rowMeans(data.frame(climate.data$tmin..deg.c., climate.data$tmax..deg.c.))
   
   # Annual Precip
   annual_precip <- aggregate(prcp..mm.day. ~ year, data = climate.data, sum)
   annual_precip <- mean(annual_precip$prcp..mm.day.)
   
   # Yearly Averages (MAT and MAP) -- MAP or AP?
   gb_yr <- group_by(climate.data, year)
   climate.year <- summarize(gb_yr, MAT = mean(tmean, na.rm = T), MAP = sum(prcp..mm.day.), MSRAD = sum(srad..W.m.2.), MIN = min(tmin..deg.c.), MAX = max(tmax..deg.c.))
   
   # Monthly Averages
   gb_m <- group_by(climate.data, year, month)
   climate.month <- summarize(gb_m, MMAT = mean(tmean, na.rm = T), MMAP = sum(prcp..mm.day.), MMSRAD = sum(srad..W.m.2.), MMIX = min(tmin..deg.c.), MMAX = max(tmax..deg.c.))
  
   # Calc Vars
   MAT <- mean(climate.year$MAT)
   MAP <- mean(climate.year$MAP)
   MSRAD <- mean(climate.year$MSRAD)
   MIN <- mean(climate.year$MIN)
   MAX <- mean(climate.year$MAX)
   Daylength_7_1 <- climate.data$dayl..s.[climate.data$year == 2018 & climate.data$yday == 182] / 60 / 60
   Daylength_12_21 <- climate.data$dayl..s.[climate.data$year == 2018 & climate.data$yday == 355] / 60 / 60
     
   # Save Results 
   climate.vars <- data.frame(latlon[j, "site"], Latitude, Longitude, MAT, MAP, MSRAD, MIN, MAX, Daylength_12_21, Daylength_7_1)
   
   # Add Rows
   climate_results <- rbind(climate_results, climate.vars) #are we binding climate_results or climate.data here?
}









