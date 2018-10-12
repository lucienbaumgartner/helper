rm(list = ls())

load('~/Twitter-SY/Twitter-SY/output/00-dataframes.RData')
source('~/r-helpers/reverse-geocoding/reverse-geocoding-fx.R')

pop <- reverse_geocode_city(data=dfl[[1]], 
                            query = 'both',
                            n.nearest.neighbours = 1)  
pop$city[!is.na(pop$geo.coord.lat)]
