function2 <- function(Individual, Nest_verdelgd, Nest_niet_verdelgd) {
  
  require(httr)
  require(sf)
  require(tidyverse)
  require(sp)
  require(raster)
  require(dplyr)
  require(gdalUtilities)
  require(tidylog)
  require(readxl)


  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  

  Individual<- Individual %>%
    #filter( inat_vv_confirmed == 1) %>%
    filter(observation_time > as.POSIXlt("2020-12-31 00:00:00"))  
  
  #source("./Scripts/link_provinces.r")
  #Individual_VL <- link_provinces(df = Individual, lat = "latitude", long = "longitude") 
  
  
  #Individual_VL_na <- Individual_VL%>%
  #  inner_join(x = Individual_VL,
  #             y = Individual%>%dplyr::select(id, latitude, longitude),
  #             by = 'id')
  
  Individual$type<-'Individu'
  Nest_verdelgd$type<-'Nest (behandeld)'
  Nest_niet_verdelgd$type<- 'Nest (niet behandeld)'
  Nest_verdelgd <- Nest_verdelgd %>% dplyr::select(inaturalist_id, longitude, latitude, type)
  Nest_niet_verdelgd <- Nest_niet_verdelgd %>% dplyr::select(inaturalist_id, longitude, latitude, type)
  
  Individual2 <- Individual %>% dplyr::select(inaturalist_id, longitude, latitude, type)
  All <- rbind(Nest_verdelgd, Nest_niet_verdelgd, Individual2)
  
  
  coord_all <- All %>% dplyr::select(longitude, latitude)
  
  
  all_spatial <- SpatialPointsDataFrame(coord_all,
                                        data = All,
                                        proj4string = crs_wgs)
  
  all_spatial<-st_as_sf(all_spatial)
  
  a<-st_distance(all_spatial$geometry, all_spatial$geometry)
  a <- drop_units(a)
  a<- as.data.frame (a)
  
  all_spatial$output <-rowSums(a<2000)#units are in m
  all_spatial$output_rel <- all_spatial$output/max(all_spatial$output)
  
  print(names(all_spatial))
  all_spatial$inaturalist_id <- as.character(all_spatial$inaturalist_id)
  names(all_spatial)[4] <- c('Type observatie')
  
  return(all_spatial)
  }