install.packages("phenocamapi")
library(tidyverse)
library(dplyr)
library(phenocamapi)
library(jpeg)
library(data.table)

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
hi <- group_by(ttower, Veg == "QURU")

# 36 sites are ACRU only, 16 are QURU only, 15 are both
