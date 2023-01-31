function1 <- function(Nest_all, Management, Overview_duplicates) {
  
  require(httr)
  require(sf)
  require(tidyverse)
  require(sp)
  require(raster)
  require(dplyr)
  require(gdalUtilities)
  require(tidylog)
  require(readxl)
  
  
  #read in data
  
  #define projection
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  WFS_mun <-"https://geoservices.informatievlaanderen.be/overdrachtdiensten/VRBG2019/wfs"
  
  
  url <- parse_url(WFS_mun)
  url$query <- list(service = "wfs",
                    #version = "2.0.0", # facultative
                    request = "GetCapabilities"
  )
  request <- build_url(url)
  request
  
  #read in sptial data layer of municipalities in flanders
  municipalities<-read_sf(request)
  
  #transform data layer to multipolygons
  ensure_multipolygons <- function(X) {
    tmp1 <- tempfile(fileext = ".gpkg")
    tmp2 <- tempfile(fileext = ".gpkg")
    st_write(X, tmp1)
    ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
    Y <- st_read(tmp2)
    st_sf(st_drop_geometry(X), geom = st_geometry(Y))
  }
  
  municipalities <- ensure_multipolygons(municipalities)
  municipalities <- st_transform(municipalities, crs=4326)
  
  #transform object to be sp compatible
  mun <- sf::as_Spatial(municipalities)
  
  nests_management <- merge(Nest_all, Management, by.x= "id", by.y="Nest (pk)", all.x=TRUE)
  
  print(unique(nests_management$Result))
  #nests_management <- nests_management %>% 
  #  mutate(outcome_nl = case_when(is.na(outcome) ~ outcome,
  #                                outcome == "PP" ~ "behandeld",
  #                                outcome == "PC" ~ "behandeld",
  #                                outcome == "FD" ~ "behandeld",
  #                                outcome == "PD" ~ "behandeld",
  #                                outcome == "ND" ~ "niet behandeld",
  #                                outcome == "UK" ~ "geen data"))
  
  Nest <- nests_management %>%
    dplyr::filter(Result == "Successfully treated")
  
  coord <- Nest %>% dplyr::select(longitude, latitude)
  
  print(head(coord))
  
  spatialdf <- SpatialPointsDataFrame(coord,
                                      data = Nest,
                                      proj4string = crs_wgs)
  
  intersect <- raster::intersect(spatialdf, mun)
  
  freq <- as.data.frame(table(intersect@data$NAAM))
  names(freq)[1] <-c('NAAM')
  municipalities <- municipalities %>% 
    full_join(x=municipalities, y=freq, by='NAAM')
  
  municipalities <- municipalities %>% 
    mutate(Freq = ifelse(is.na(Freq), 0, Freq))
  
  return (municipalities)
  }
