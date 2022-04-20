# Process for cleaning gbif geographical data 
# Data for Pteridium aquilinum data downloaded from GBIF, query citation:
# GBIF.org (19 April 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.thfs3r

# Libraries:
# Devtools needed for some instalations.
library(devtools)
library(tidyverse)
library(countrycode)
library(CoordinateCleaner)
library(rgbif)
library(sp)


# obtain data from GBIF via rgbif - see help restrictions on batch queries:
# dat <- occ_search(scientificName = "Pteridium aquilinum", limit = 5000, hasCoordinate = T)

dat <- read_csv(r"(data\Pteridium_aquilinum_gbif.csv)")

# select columns of interest
dat <- dat %>% select(species, decimalLongitude, decimalLatitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode) # datasetName

# remove records without coordinates (coordinates as NA)
dat <- dat %>% filter(!is.na(decimalLongitude)) %>% filter(!is.na(decimalLatitude))

# plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")

ggplot() + 
  coord_fixed() + 
  wm + 
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude), colour = "green", size = 0.5) + 
  theme_bw()

# convert country code from ISO2c to ISO3c
dat$countryCode <- countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

# flag problems
dat <- data.frame(dat) # function needs data as data frame

flags <- clean_coordinates(x = dat, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           # Most of the following are on by default, separated by /n for commenting/uncommenting: 
                           tests = c("capitals", 
                                     "centroids", 
                                     "equal",
                                     "gbif", 
                                     "institutions",
                                     "zeros",
                                     "duplicates", 
                                     "seas", 
                                     "urban", 
                                     "validity"))

summary(flags)

# make plot with flagged points:
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

# exclude problematic records (~20k)
dat_cl <- dat[flags$.summary,]

# save flagged records (~6k)
dat_fl <- dat[!flags$.summary,]

# Temporal outliers. Not clear why ~3.5k records are flagged (flagged years include: 2019, 2020. 2021)
flags <- cf_age(x = dat_cl,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                taxon = "species", 
                min_age = "year", 
                max_age = "year", 
                value = "flagged")

table(dat_cl[flags, "year"])
table(dat_cl[!flags, "year"])

# Excluding data prior to 1945:
dat_cl <- dat_cl %>% filter(year >= 1945)

# Exporting resulting dataframe as csv and only with species, lat and long columns:
dat_cl <- dat_cl %>% select(species, decimalLongitude, decimalLatitude)

write.csv(dat_cl, r"(data\pteridium_aquilinus_clean.csv)")

# Session info
sessionInfo()

