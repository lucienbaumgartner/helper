library(dplyr)
rm(list=ls())

geonames.df <- read.table('~/Twitter-SY/Twitter-SY/input/cities1000.txt', 
                     sep = '\t', fill = TRUE, header=F, stringsAsFactors = F) %>% 
  as_tibble

geonames.coord <- select(cities, lat=V5, lng=V6, id=V1) %>% 
  mutate_all(., as.numeric) %>% 
  na.omit


save(geonames.df, file = '~/r-helpers/reverse-geocoding/input/0x-geonames-cities1000.RData')
save(geonames.coord, file = '~/r-helpers/reverse-geocoding/input/0x-geonames-cities1000-coord-source-reverse-geocoding.RData')
