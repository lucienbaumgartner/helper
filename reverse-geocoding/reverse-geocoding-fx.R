library(rgeos)
library(sp)
library(reshape2)
library(pbmcapply)
library(RANN)
library(dplyr)

rm(list=ls())

load('~/Twitter-SY/Twitter-SY/output/00-dataframes.RData')
cities <- read.table('~/Twitter-SY/Twitter-SY/input/cities1000.txt', 
                     sep = '\t', fill = TRUE, header=F, stringsAsFactors = F) %>% 
  as_tibble

cities[cities$V5==''|cities$V6=='',]
c.coord <- select(cities, lat=V5, lng=V6, id=V1) %>% 
  mutate_all(., as.numeric) %>% 
  na.omit

str(dfl[2])
df <- dfl[[1]] %>% 
  select(id, 
         geo.coordinates.0,
         geo.coordinates.1, 
         grep('bounding_box', names(dfl[[1]]), value = T)) %>% 
  setNames(., c('id', 
                'geo.coord.lat', 
                'geo.coord.lng', 
                paste0(
                  'bounding_box_',
                  grep('bounding_box', names(dfl[[1]]), value = T) %>% 
                         gsub('.*[A-z]{1,}\\.0\\.', '', .) %>% 
                         as.numeric %>% 
                         format(as.vector(as.vector(.)+1), digits = 1) %>% 
                         gsub('0$', 'lng', .) %>% 
                         gsub('1$', 'lat', .)
                  )
                )
           ) %>% 
  mutate(city=ifelse(is.na(geo.coord.lat)|is.na(geo.coord.lng), 0, 1)) %>% 
  as_tibble

dfs <- df %>% 
  filter(city==2) 

dfst <- tibble(lat=c(t(dfs[grepl('bounding_box.*\\.lat', names(dfs))])), 
  lng=c(t(dfs[grepl('bounding_box.*\\.lng', names(dfs))])),
  group=rep(c(1:(length(lng)/4)), each=4)) %>% 
  split.data.frame(., .$group) %>%
  pbmclapply(., function(x) select(x, -group), mc.cores = 4)

print('[STATUS] bounding boxes: set up')
  
dfst <- pbmclapply(dfst, function(x){
  tmp <- Polygon(x) 
  tmp <- Polygons(list(tmp), 1) 
  tmp <- SpatialPolygons(list(tmp))
  return(tmp)
}, mc.cores = 4)

print('[STATUS] polygons: set up')

centroids <- pbmclapply(dfst, gCentroid, mc.cores = 4)

dfs <- dfs %>% 
  mutate(centroids=centroids)

cities <- read.table('~/Twitter-SY/Twitter-SY/input/cities1000.txt', 
                       sep = '\t', fill = TRUE, header=F, stringsAsFactors = F) %>% 
  as_tibble

cities[cities$V5==''|cities$V6=='',]
c.coord <- select(cities, lat=V5, lng=V6, id=V1) %>% 
  mutate_all(., as.numeric) %>% 
  na.omit

centroids <- lapply(centroids, as.data.frame) %>% 
  do.call(rbind, .)
nns <- nn2(data = select(c.coord, -id), query = centroids)
nns$nn.idx[nns$nn.dists==min(nns$nn.dists)] %>% unique

id <- c.coord[nns$nn.idx[,1] %>% unique,]
cities %>% filter(V1==id$id)

as.numeric(rownames(nns$nn.idx[nns$nn.dists==min(nns$nn.dists),]))

library(Rfast)

rowMins(nns$nn.dists, value = F)
mins <- apply(nns$nn.dists, 1,)
as_data_frame(nns$nn.dists)
Mins <- sapply(1:length(mins), function(x) [x,]%in%mins[x])
nns$nn.idx[nns$nn.dists%in%mins]
nns$nn.dists %>% 
  t
  as_tibble %>% 
  rowwise %>% 
  summarise_all(., min)
  mutate_all(., min)

             