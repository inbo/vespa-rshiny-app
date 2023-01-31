function3 <- function (Nest_all, Individual, Management, Overview_queens, Lege_nesten, foutieve_nesten, foutieve_observatie, verdelgd, duplicates_obs) {
  
  require(httr)
  require(sf)
  require(tidyverse)
  require(sp)
  require(raster)
  require(dplyr)
  require(gdalUtilities)
  require(tidylog)
  require(readxl)

  #Remove empty nests from the year before
  Nest_all <- Nest_all %>% 
    dplyr::filter(!inaturalist_id %in% Lege_nesten$inaturalist_id)%>%
    dplyr::filter(!inaturalist_id %in% foutieve_nesten$inaturalist_id)
  
  Individual<- Individual %>%
    #filter( inat_vv_confirmed == 1) %>%
    dplyr::filter(!inaturalist_id %in% foutieve_observatie$inaturalist_id)%>%
    filter(observation_time > as.POSIXlt("2021-1-1 00:00:00")) %>%
    dplyr::filter(!inaturalist_id %in% Overview_queens$inaturalist_id)
  
  nests_management <- merge(Nest_all, Management, by.x= "id", by.y="Nest (pk)", all.x=TRUE)
  
  #nests_management <- nests_management %>% 
  #  mutate(outcome_nl = case_when(is.na(outcome) ~ "ongekend",
  #                                outcome == "PP" ~ "behandeld",
  #                                outcome == "PC" ~ "behandeld",
  #                                outcome == "FD" ~ "behandeld",
  #                                outcome == "PD" ~ "behandeld",
  #                                outcome == "ND" ~ "niet behandeld",
  #                                outcome == "UK" ~ "geen data"))
  
  Nest_behandeld <- nests_management %>%
    dplyr::filter(Result == "Successfully treated")
  
  #Niet behandelde nesten worden weergegeven
  Nest_niet_behandeld <- nests_management %>%
    dplyr::filter(Result != "Successfully treated" | is.na(Result)) %>%
    dplyr::filter(`Nest reported before` != TRUE)
    
  
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  #transformeren van data in ruimtelijke objecten
  
  ##observaties
  
  obs <- Individual %>% dplyr::select(longitude, latitude)
  all_spatial_obs <- SpatialPointsDataFrame(obs,
                                            data = Individual,
                                            proj4string = crs_wgs)
  all_spatial_obs<-st_as_sf(all_spatial_obs)
  
  
  ##behandelde nesten
  Nest_b<- Nest_behandeld %>% dplyr::select(longitude, latitude)
  all_spatial_Nest_b <- SpatialPointsDataFrame(Nest_b,
                                               data = Nest_behandeld,
                                               proj4string = crs_wgs)
  all_spatial_Nest_b<-st_as_sf(all_spatial_Nest_b)
  
  ##niet behandelde nesten
  Nest_nb <- Nest_niet_behandeld %>% dplyr::select(longitude, latitude)
  all_spatial_Nest_nb <- SpatialPointsDataFrame(Nest_nb,
                                                data = Nest_niet_behandeld,
                                                proj4string = crs_wgs)
  all_spatial_Nest_nb<-st_as_sf(all_spatial_Nest_nb)
  
  #bereken afstand tussen alle reeds behandelde nesten en observaties
  proximity <- st_distance(all_spatial_obs$geometry, all_spatial_Nest_b$geometry)
  proximity <- drop_units(proximity)
  proximity <- as.data.frame(proximity)
  #a vervangen met Yes en No
  
  proximity <- proximity %>% 
    replace(proximity <=2000, 1) %>%
    replace(proximity >2000, 0)
  
  names(proximity) <- all_spatial_Nest_b$id
  
  #bereken timing tussen alle behandelde nesten en observaties
  
  timing <- Individual %>%
    select(id, observation_time)
  
  print(timing)
  print(Nest_behandeld)
  
  for(nest in Nest_behandeld$id) {
    print(nest)
    print(Nest_behandeld[which(Nest_behandeld$id == nest), 'action_time'])
    timing <- within (assign("timing"  , get("timing")) ,
                      assign(as.character(nest), Nest_behandeld[ which(Nest_behandeld$id == nest), 'Date and time nest removal'] >= timing$observation_time)         
    )
  }
  
  timing <- timing %>%
    select(-c(id, observation_time))
  
  timing <- as.data.frame(timing)
  
  timing <- timing %>% 
    replace(timing == TRUE, 1) %>%
    replace(timing == FALSE, 0)
  
  #welke observaties voldoen aan beide voorwaarden?
  #voorwaarde1: in nabijbheid van nest (binnen straal van 2km)
  #voorwaarde2: geobserveerd voordat dat nest werd verwijderd
  
  array_prox <- as.matrix(proximity)
  array_timing <- as.matrix(timing)
  
  array_total <- array_prox * array_timing
  Individual$delete <- apply(array_total, 1, max)
  
  #observaties die aan beide voorwaarden voldoen worden verwijderd
  Individual <- Individual %>% 
    dplyr::filter(delete == 0)
  
  
  #####################DISPLAY HEATMAP#######################
  Individual$type<-'Individu'
  Nest_niet_behandeld$type<-'Nest'
  Nest_niet_behandeld <- Nest_niet_behandeld %>% dplyr::select(inaturalist_id, longitude, latitude, type)
  Individual2 <- Individual %>% dplyr::select(inaturalist_id, longitude, latitude, type)
  All <- rbind(Nest_niet_behandeld, Individual2)
  
  coord_all <- All %>% dplyr::select(longitude, latitude)
  
  
  all_spatial_AH <- SpatialPointsDataFrame(coord_all,
                                           data = All,
                                           proj4string = crs_wgs)
  
  all_spatial_AH<-st_as_sf(all_spatial_AH)
  
  a<-st_distance(all_spatial_AH$geometry, all_spatial_AH$geometry)
  a <- drop_units(a)
  a<- as.data.frame (a)
  
  all_spatial_AH$output <-rowSums(a<2000)#units are in m
  all_spatial_AH$output_rel <- all_spatial_AH$output/max(all_spatial_AH$output)
  
  print(names(all_spatial_AH))
  names(all_spatial_AH)[4] <- c('Type observatie')
  
  return(all_spatial_AH)
  }
