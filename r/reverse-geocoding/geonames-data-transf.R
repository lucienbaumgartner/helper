library(dplyr)
rm(list=ls())

l.gn <- readLines('~/Twitter-SY/Twitter-SY/input/cities1000.txt')
l.gn <- strsplit(l.gn, '\\t')

geonames.df <- tibble(id=as.numeric(sapply(l.gn, '[[', 1)),
       name=sapply(l.gn, '[[', 2),
       lat=as.numeric(sapply(l.gn, '[[', 5)),
       lng=as.numeric(sapply(l.gn, '[[', 6)))

geonames.coord <- select(geonames.df, -name) 

save(geonames.df, file = '~/r-helpers/reverse-geocoding/input/0x-geonames-cities1000.RData')
save(geonames.coord, file = '~/r-helpers/reverse-geocoding/input/0x-geonames-cities1000-coord-source-reverse-geocoding.RData')
