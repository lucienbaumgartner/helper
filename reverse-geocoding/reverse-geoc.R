reverse_geocode_city <- function(data, query, geo.source.coordinates, geo.source.full, n.nearest.neighbours){
  df <- data %>% 
    select(id, 
           geo.coordinates.0,
           geo.coordinates.1, 
           grep('bounding_box', names(data), value = T)) %>% 
    setNames(., c('id', 
                  'geo.coord.lat', 
                  'geo.coord.lng', 
                  paste0(
                    'bounding_box_',
                    grep('bounding_box', names(data), value = T) %>% 
                      gsub('.*[A-z]{1,}\\.0\\.', '', .) %>% 
                      as.numeric %>% 
                      format(as.vector(as.vector(.)+1), digits = 1) %>% 
                      gsub('0$', 'lng', .) %>% 
                      gsub('1$', 'lat', .)
                  )
    )
    ) %>% 
    mutate(coord.L=ifelse(is.na(geo.coord.lat)|is.na(geo.coord.lng), F, T)) %>% 
    as_tibble
  
  if(identical(query, 'bounding_box')){
    df.base <- filter(df, coord.L==F)
    query.lng <- paste0(query, '.*\\.lng')
    query.lat <- paste0(query, '.*\\.lat')
    n.points.polygon <- length(grep('bounding_box', names(data)))
    
    l.query <- tibble(
      lat=c(t(df.base[grepl(query.lat, names(df.base))])), 
      lng=c(t(df.base[grepl(query.lng, names(df.base))])),
      group=rep(c(1:(length(lng)/4)), each=4)) %>% 
      split.data.frame(., .$group) %>%
      pbmclapply(., function(x) select(x, -group), mc.cores = 4)
    
    print('[STATUS] bounding boxes: set up')
    
    sp.poly.query <- pbmclapply(l.query, function(x){
      tmp <- Polygon(x) 
      tmp <- Polygons(list(tmp), 1) 
      tmp <- SpatialPolygons(list(tmp))
      return(tmp)
    }, mc.cores = 4)
    
    print('[STATUS] polygons: computed')
    
    sp.centroids <- pbmclapply(sp.poly.query, gCentroid, mc.cores = 4)
    
    query <- lapply(sp.centroids, as.data.frame) %>% 
      do.call(rbind, .)
    
    df.base <- df.base %>% 
      mutate(sp.centroids=sp.centroids)
    
    print('[STATUS] centroids: computed')
  }
  if(identical(query, 'geo.coord')){
    df.base <- filter(df, coord.L==T) 
    query <- as.data.frame(x=geo.coord.lat, y=geo.coord.lng)
  }
  
  nns <- nn2(data = select(geo.source.coordinates, -id), query = query)
  
  ids <- geo.source.coordinates$id[nns$nn.idx[,1:n.nearest.neighbours]]
  
  city <- geo.source.full$V2[geo.source.full$V1 %in% ids]
  
  df.base <- as_tibble(cbind(df.base, city=city))
  
  df <- left_join(df, df.base, by='id')
  
  print('[STATUS] nearest neighbours: computed')
  
  return(df)
}

pop <- reverse_geocode_city(data=dfl[[1]], query = 'bounding_box', geo.source.coordinates = c.coord, geo.source.full=cities, n.nearest.neighbours = 1)  
head(pop$city)
