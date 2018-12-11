# Reverse geocoding: An emulation of the python-based library GeoCoder in R

The function located in `reverse-geocoding-fx.R` emulates the [geoCoder library](https://github.com/DenisCarriere/geocoder)

## Function

The function automatically loads the geonames dataset for cities with population > 10000 as a source for the reverse geocoding. It is also possible to use another source.

The function allows to specify the following parameters:

- `data`
- `query`: either only `bounding_box` data, only `geo.coord`, or `both`
- `geo.source.coordinates`; default: geoNames data
- `geo.source.meta`; default: geoNames data
- `n.nearest.neighbours`: number of nearest numbers; if > 1, returned as list
- `n.cores`: number of cores available for parallelization; default: half of total cores

```
function(data, query, geo.source.coordinates = geonames.coord, geo.source.full = geonames.df, n.nearest.neighbours, n.cores = NULL){
  if(is.null(n.cores)) n.cores <- detectCores()-(round(detectCores()/2, 0))
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

  if(identical(query, 'both')){
    l.both <- lapply(c('bounding_box', 'geo.coord'), function(query){
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
          pbmclapply(., function(x) select(x, -group), mc.cores = n.cores)

        print('[STATUS] bounding boxes: set up')

        sp.poly.query <- pbmclapply(l.query, function(x){
          tmp <- Polygon(x)
          tmp <- Polygons(list(tmp), 1)
          tmp <- SpatialPolygons(list(tmp))
          return(tmp)
        }, mc.cores = n.cores)

        print('[STATUS] polygons: computed')

        sp.centroids <- pbmclapply(sp.poly.query, gCentroid, mc.cores = n.cores)

        query <- lapply(sp.centroids, as.data.frame) %>%
          do.call(rbind, .)

        # df.base <- df.base %>% mutate(sp.centroids=sp.centroids)

        print('[STATUS] centroids: computed')

        return(list(df.base, query))
      }
      if(identical(query, 'geo.coord')){
        df.base <- filter(df, coord.L==T)
        query <- tibble(x=df.base$geo.coord.lat, y=df.base$geo.coord.lng)

        print('[STATUS] geocoordinates: extracted')

        return(list(df.base, query))
      }
    })

    df.base <- lapply(l.both, '[[', 1) %>% do.call(rbind, .)
    query <- lapply(l.both, '[[', 2) %>% do.call(rbind, .)
  }else{
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
        pbmclapply(., function(x) select(x, -group), mc.cores = n.cores)

      print('[STATUS] bounding boxes: set up')

      sp.poly.query <- pbmclapply(l.query, function(x){
        tmp <- Polygon(x)
        tmp <- Polygons(list(tmp), 1)
        tmp <- SpatialPolygons(list(tmp))
        return(tmp)
      }, mc.cores = 4)

      print('[STATUS] polygons: computed')

      sp.centroids <- pbmclapply(sp.poly.query, gCentroid, mc.cores = n.cores)

      query <- lapply(sp.centroids, as.data.frame) %>%
        do.call(rbind, .)

      df.base <- df.base %>%
        mutate(sp.centroids=sp.centroids)

      print('[STATUS] centroids: computed')
    }
    if(identical(query, 'geo.coord')){
      df.base <- filter(df, coord.L==T)
      query <- tibble(x=df.base$geo.coord.lat, y=df.base$geo.coord.lng)
    }
  }

  nns <- nn2(data = select(geo.source.coordinates, -id), query = query)

  ids <- geo.source.coordinates$id[nns$nn.idx[,1:n.nearest.neighbours]]

  city <- tibble(city.id=as.numeric(geo.source.full$id[geo.source.full$id%in%ids]),
                 city=geo.source.full$name[geo.source.full$id%in%ids],
                 city.lat=as.numeric(geo.source.full$lat[geo.source.full$id%in%ids]),
                 city.lng=as.numeric(geo.source.full$lng[geo.source.full$id%in%ids])) %>%
    left_join(tibble(city.id=ids), ., by='city.id')

  df.base <- as_tibble(cbind(df.base, city))

  df <- left_join(df, df.base, by=names(df.base)[names(df.base)%in%names(df)])

  print('[STATUS] nearest neighbours: computed')

  return(df)
}
```


## Functionality

For all cases, in which only bounding boxes are available but no precise geo coordinates, the function (i) sets up the bounding boxes, (ii) computes the polygons, and subsequently their (iii) centroids. Those are used then used to compute the nearest neighbours. When geo coordinates are available, they extracted and directly used to compute the nearest neighbours. If both options shall be used (like in our case), it will do both. Nearest neighbour results based on geo coordinates will always override bounding box-only results.  Ultimately, the function returns the same dataframe that was fed to it, complemented with the new geoinformation. At the moment, the function only supports these new variables:`city.id` (geoNames), `city` (name), `city.lat`, `city.lng`. The coordinates are returned for validation purposes (see below). I'll add country infomration, asap.

In the following there is a test run with the Syria Twitter Data 2015. It shows that the python library (reverseGeocoder) and my function produce the exact same results in 98.87% of all cases.

In 68.96% of the remaining cases (those that differ), my function maps them more accurately than the python library.
