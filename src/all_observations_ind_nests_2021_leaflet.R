#data visualisation of distribution data in 2020 in leaflet
library("leaflet")
library("tidyverse")
library("leaflet.extras")
library("sf")
library("units")
library("mapview")
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(rgeos)
library(leaflet.extras)
library(units)
library(httr)
library(sf)
library(tidyverse)
library(sp)
library(raster)
library(dplyr)
library(gdalUtilities)
library(tidylog)
library(readxl)

Nest <- read_csv("https://www.vespawatch.be/api/csv_export/vv_confirmed_nests/")
names(Nest)<- c('id', 'observation_time', 'species', 'latitude', 'longitude', 'originates_in_vespawatch', 'inaturalist_id', 'height', 'size')
Management <- read_csv("https://www.vespawatch.be/api/csv_export/management_actions")
Overview_duplicates <- read_excel("data/Overview_duplicates.xlsx", 
                                  sheet = "duplicates")
Individual <- read_csv("https://www.vespawatch.be/api/csv_export/vv_confirmed_individuals/")
names(Individual) <- c('id', 'observation_time', 'species', 'latitude', 'longitude', 'originates_in_vespawatch', 'inaturalist_id', 'individual_count', 'behaviour')
Lege_nesten <- read_csv("data/leeg_nest_vorig_jaar.csv")
duplicates_obs <-read_csv("data/duplicates_obs.csv")
Overview_queens <- read_delim("data/Overview_queens.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
foutieve_nesten<-read_csv("data/foutieve_nesten.csv")
foutieve_observatie <- read_csv("data/foutieve_observatie.csv")
verdelgd <- read_csv("data/verdelgd.csv")

#PART2: preprocessing of data
##Remove duplicates Karel
Nest <- Nest %>% dplyr::filter(!inaturalist_id %in% Overview_duplicates$duplicaten)

##Only Vespa Velutina and in 2021
Nest_all<- Nest %>%
  #dplyr::filter(inat_vv_confirmed == 1) %>%
  dplyr::filter(observation_time > as.POSIXlt("2021-1-1 00:00:00"))%>%
  dplyr::select(id, inaturalist_id, longitude, latitude)


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
Nest_all$type<-'Nest'
Nest_all <- Nest_all %>% dplyr::select(inaturalist_id, longitude, latitude, type)
Individual2 <- Individual %>% dplyr::select(inaturalist_id, longitude, latitude, type)
All <- rbind(Nest_all, Individual2)


coord_all <- All %>% dplyr::select(longitude, latitude)


all_spatial <- SpatialPointsDataFrame(coord_all,
                                      data = All,
                                      proj4string = crs_wgs)

all_spatial<-st_as_sf(all_spatial)



print(names(all_spatial))
all_spatial$inaturalist_id <- as.character(all_spatial$inaturalist_id)
names(all_spatial)[4] <- c('Type observatie')
leaflet(all_spatial)%>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   color= ~pal(all_spatial$`Type observatie`),
                   opacity=0.5,
                   radius=5,
                   popup =  paste0("<a href=https://www.inaturalist.org/observations/", all_spatial$inaturalist_id, ">", "iNaturalist", "</a>")) %>%
  addLegend(pal = pal,
            values= ~`Type observatie`,
            opacity = 0.7,
            position = "bottomright")%>%
  addScaleBar(position = "bottomleft")




mapshot(leaflet_heatmap_no_circles, url = paste0(getwd(), "/map_no_circles.html"))
