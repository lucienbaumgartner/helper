library(stringr)
library(dplyr)

rm(list=ls())

source('~/r-helpers/url-snowballing/url-snowballing-fx.R')

cycler(start='https://www.uvek.admin.ch/uvek/de/home/uvek/abstimmungen/no-billag-initiative.html', 
             time.limit=60, 
             keywords='no?(-)billag')


