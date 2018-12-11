library(stringr)
library(dplyr)

rm(list=ls())

source('~/r-helpers/url-snowballing/url-snowballing-fx.R')

cycler(start='https://www.admin.ch/gov/de/start/dokumentation/abstimmungen/20180304/initiative-no-billag.html', 
             time.limit=10, 
             keywords='no?(-)billag')


cycler(start='https://www.admin.ch/gov/de/start/dokumentation/abstimmungen/20180304/initiative-no-billag.html', 
       time.limit=10, 
       keywords=NULL)
