# LOADING LIBRARIES
library(tidyverse)
library(readr)

# CALLING THE DATASET TO R
hiv <- read_csv("HIV data 2000-2023.csv")
# View(hiv)
# hiv |> glimpse()

# REMOVING DUPLICATES (if any)
hiv <- hiv[!duplicated(hiv),]

## Selecting relevant columns
hiv <- hiv[,-c(1:3, 6, 9)] |> 
  rename("regionCode" = ParentLocationCode,"region"=ParentLocation,
         "countryCode"=SpatialDimValueCode,"country"=Location,
         "year"=Period, "estimate"=Value)

## point estimate variable
hiv$p.estimate <- hiv$estimate |> 
  str_remove(pattern = " ") |> 
  str_extract(pattern = "\\d+") 

## lower-bound
hiv$lowerB <- hiv$estimate |>
  str_remove_all(pattern = " ") |>
  str_extract(pattern = "(?<=\\[)\\d+")


## Upper-bound
hiv$upperB <- hiv$estimate |>
  str_remove_all(pattern = " ") |>
  str_extract(pattern = "(?<=-)\\d+(?=\\])")



# REMOVING ALL RECORDS WITHOUT A POINT ESTIMATE
hiv <- hiv[!is.na(hiv$p.estimate), ]


# Streamlining lower and upper bounds
## lower bounds
hiv[is.na(hiv$lowerB),]$lowerB <- "0"

## upper bounds 
for(i in hiv[is.na(hiv$upperB),]$upperB){
  for(j in hiv[is.na(hiv$upperB),]$p.estimate){
  hiv[is.na(hiv$upperB),]$upperB <- j
 }
}

# NUMERICAL VARIABLES
hiv$p.estimate <- as.numeric(hiv$p.estimate)
hiv$lowerB <- as.numeric(hiv$lowerB)
hiv$upperB <- as.numeric(hiv$upperB)

# FACTORIAL VARIABLES
hiv$year <- as.factor(hiv$year)
hiv$regionCode <- as.factor(hiv$regionCode)
hiv$region <- as.factor(hiv$region)
hiv$countryCode <- as.factor(hiv$countryCode)
hiv$country <- as.factor(hiv$country)

# WRITING TO CSV
# write_csv(hiv, "hiv_clean.csv")


