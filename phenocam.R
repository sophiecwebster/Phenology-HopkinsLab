############################################
######## PHENOCAM DATA RETRIEVAL ###########
############################################

# Load packages
library(tidyverse)
library(phenocamapi)
library(rgdal)
library(sp)
library(dplyr)
library(jpeg)
library(data.table)
library(fields)
library(raster)
library(phenopix)
library(data.table)

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

# obtaining the list of all the available ROI's on the PhenoCam server
rois <- get_rois()

sites_rois <- rois %>% right_join(all.sites, by = c("site" = "site", "lat" = "lat", "lon" = "lon"))

ts_sites <- rois %>% right_join(all.sites, by = c("site" = "site", "lat" = "lat", "lon" = "lon")) %>%
    filter(!is.na(roitype))


#for loop it! do we want this down to 1 day or 3 day res? 3 day is fine
# going thru everything but not saving it all -- only last guy
# want site name to be level name of this mega factor? i think?

time_series <- c()
for(i in 1:nrow(sites_rois)) {
  site <- ts_sites$site[i]
  ts <- get_pheno_ts(site = ts_sites$site[i], vegType = ts_sites$roitype[i], roiID = ts_sites$sequence_number[i], type = '3day')
ts$Site_Name <- site
time_series <- rbind(time_series, ts)
}

# Pulling out only date, year, doy, gcc_mean, midday gcc, gcc 90, and site name
trimmed <- time_series[,c(1:3,9,17,21,35,36)]

# Creating smaller subsection from January to September 2013 to test out on (it has all GCC midday values intact)
### Can I safely remove all NAs? ###
thirteen <- trimmed[c(85:162),c(3:4)]

# Fitting Adapted from Willamette; need to make a zoo object so phenopix can read it (just organizational formality)
zoom <- zoo(thirteen$midday_gcc, order.by = index(thirteen$doy), frequency=NULL)
k_fit <- KlostermanFit(zoom, which = "light", uncert = T, nrep = 100, ncores = 4)

# Extract PhenoPhases
pp_kl <- PhenoExtract(k_fit, method = "klosterman", uncert = T)

# now just checking out alligatorriver site to see how it looks/graphin'
gator <- get_pheno_ts(site = sites_rois$site[1], vegType = sites_rois$roitype[1], roiID = sites_rois$sequence_number[1], type = '1day') %>% 
    filter(!is.na(gcc_90))

gator$date <- as.Date(gator$date)

gator %>% ggplot(aes(date, gcc_90)) + geom_point(alpha = 0.2, color = "green") + labs(x = "", y = "GCC 90") +
  scale_x_date(date_breaks = "1 year")

springyd <- gator$date[1604:1674]
gspringy <- gator$gcc_90[1604:1674]
springyd <- as.Date(springyd)
hallo <- cbind(springyd, gspringy)
hallo <- as.data.frame(hallo)

hallo %>% ggplot(aes(springyd, gspringy)) + geom_point(alpha = 0.5, color = "green") + labs(x = "", y = "GCC 90") #+
  #scale_x_date()


# eventually:
# need to convert from factor to date
var_[,date:=as.Date(date)]


# BIG QUESTION: how to figure out when leaf-out is? when dGCC/dT is highest?

# find the mean winter minimum and the mean summer maximum. Subtract the winter
# value from the summer value. Find 10% of the difference and add it to the
# winter value- this should be the GCC value of the day of budburst
# https://phenocam.sr.unh.edu/education/U3_PhenoCamData_Activity_GraphingAnalyzingPhenoCamData_Final.pdf
# https://www.biorxiv.org/content/biorxiv/early/2019/09/18/771477.full.pdf
# above, "u" was the proxy for budburst, pg 6-7

