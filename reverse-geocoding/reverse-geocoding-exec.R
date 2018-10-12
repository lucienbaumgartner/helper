rm(list = ls())

load('~/Twitter-SY/Twitter-SY/output/00-dataframes.RData')
source('~/r-helpers/reverse-geocoding/reverse-geocoding-fx.R')

pop <- reverse_geocode_city(data=dfl[[5]][1:400,], 
                            query = 'both',
                            n.nearest.neighbours = 1)  
pop[!is.na(pop$geo.coord.lat),] %>% data.frame

