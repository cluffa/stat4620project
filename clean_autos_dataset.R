# This script cleans and formats the autos dataset
# inputs: autos.Rdata, Germany_complete_gis_info.csv
# outputs: clean_autos.Rdata

library(tidyr)
library(dplyr)
library(readr)

load("autos.Rdata")

# dataset from kaggle used to match locations to postal code:
# https://www.kaggle.com/ravikanth/germany-postal-codes-gis-city-district-and-state
germany.info <- read_csv("Germany_complete_gis_info.csv") %>% 
  transmute(
    postalCode = as.character(`Postal Code`),
    state = `State`,
    district = District
    ) %>% 
  distinct()

test$train = FALSE
train$train = TRUE

autos <- bind_rows(test, train) %>% 
  select(
    -nrOfPictures,
    -name,
    -abtest
  ) %>% 
  filter(
    yearOfRegistration < 2022,
    yearOfRegistration > 1900,
    powerPS < 1000,
    price > 0,
    price <= 500000
  ) %>% 
  mutate(
    dateCreated = as.Date(dateCreated),
    lastSeen = as.Date(lastSeen),
    dateCrawled = as.Date(dateCrawled),
    daysOnline = as.numeric(lastSeen - dateCreated),
    postalCode = as.factor(postalCode),
    monthOfRegistration = as.factor(monthOfRegistration),
    kilometer = as.factor(kilometer)
  ) %>% 
  left_join(germany.info, by = "postalCode") %>% 
  mutate_if(is.character, function(word) ifelse((word == "") | (is.na(word)), "NA", word))  %>% 
  mutate_if(is.character, as.factor)

autos$kilometer <- recode_factor(autos$kilometer, `150000` = "more than 150000")

# in progress 
# combines least used factors to "other"
# for the purpose of using in models with factor level limit

# ordered <- autos %>% 
#   group_by(brand) %>% 
#   summarise(count = n()) %>% 
#   arrange(count) %>% 
#   transmute(brand = as.character.factor(brand)) %>% 
#   top_n(10)
# 
# autos$brand.collapsed <- autos$brand
# forcats::fct_collapse(autos$brand.collapsed, other = ordered$brand)

test <- autos %>% filter(train == FALSE) %>% select(-train)
train <- autos %>% filter(train == TRUE) %>% select(-train)
rm(autos, germany.info)
save(test, train, file = "clean_autos.Rdata")
