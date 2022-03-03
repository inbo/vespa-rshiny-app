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


#PART1: read in data
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

nests_management <- merge(Nest, Management, by.x= "id", by.y="Nest (pk)", all.x=TRUE)

Nest_temp <- nests_management %>%
  dplyr::filter(`Nest reported before` != TRUE |is.na(`Nest reported before`))%>%
  dplyr::filter(observation_time > as.POSIXlt("2021-1-1 00:00:00"))


Nest_verdelgd <- Nest_temp%>%
  dplyr::filter(Result ==  'Successfully treated')%>%
  dplyr::select(id, inaturalist_id, longitude, latitude)

Nest_niet_verdelgd <- Nest_temp %>%
  dplyr::filter(Result !=  'Successfully treated')%>%
  dplyr::select(id, inaturalist_id, longitude, latitude)
  

Nest_all <- Nest_temp %>%
  dplyr::select(id, inaturalist_id, longitude, latitude)

##apply preprocessing functions
source("./scripts/function1.R")
municipalities <- function1(Nest_all, Management, Overview_duplicates)

source("./scripts/function2.R")
all_spatial <- function2(Individual, Nest_verdelgd, Nest_niet_verdelgd)

source("./scripts/function3.R")
all_spatial_AH <- function3(Nest_all, Individual, Management, Overview_queens, Lege_nesten, foutieve_nesten, foutieve_observatie, verdelgd, duplicates_obs)

source("./scripts/function4.R")
Brussel_shape <- function4()


#PART3: CREATE APP

##define userinterface
ui <- navbarPage(
                 title= "Vespa-Watch waarnemingen in 2021", 
                 header= fluidRow(
                   box(width = 12, 
                       img(src='INSTvoorNatuur.jpg', align= 'left', height = 50),
                       img(src='logo_vespawatch.png', align= 'right', height = 50)),
                   box( width = 12, align ='right',
                        tags$a(href="https://vespawatch.be/", '   www.vespawatch.be'),
                        ', info@vespawatch.be')
                 ),tabPanel(
                   title = "Alle observaties", width = 12,
                   h3('Alle waarnemingen van de Aziatische hoornaar in Vlaanderen in 2021'),
                   "Op deze kaart is de verspreiding afgebeeld van alle nesten en individuen van de Aziatische hoornaar die in 2021 zijn gemeld op vespawatch.be. Klik op een waarneming voor de bijhorende link op iNaturalist.",
                   br(),
                   leafletOutput("heatmap", height = 600)
                 ),
                 tabPanel(
                   title = "Actieve haarden", width = 12, 
                   h3('Actieve haarden van de Aziatische hoornaar in Vlaanderen in 2021'),
                   "Deze kaart illustreert de verspreiding van actieve haarden van de Aziatische hoornaar in Vlaanderen. In de buurt van deze waarnemingen bevindt zich een nest binnen een straal van 2 km, dit is de maximale afstand dat een Aziatische hoornaar zich verplaatst vanaf zijn nest. Op deze plaatsen vinden mogelijks opsporingsacties plaats door de", tags$a(href="https://www.facebook.com/groups/474218836579263", "Vespawatchers"),
                   ". Hoe intenser rood, hoe meer meldingen er in een radius van 2 km rond een waarneming op vespawatch.be werden gemeld. Reeds gevonden nesten die nog moeten worden verwijderd zijn ook afgebeeld. Klik op een waarneming voor de bijhorende link op iNaturalist.",
                   br(),leafletOutput("actieve_haarden", height = 600)
                 ),
                 tabPanel(
                   title = "Per gemeente", width = 12,
                   h3('Behandelde nesten van de Aziatische hoornaar in Vlaanderen in 2021'),
                   "Deze kaart geeft een overzicht van het aantal behandelde nesten per gemeente in 2021.",
                   br(),
                   leafletOutput("management", height = 600),
                   tags$a(href="https://metadata.vlaanderen.be/srv/dut/catalog.search#/metadata/9ff44cc4-5f16-4507-81a6-6810958b14df", "Bron: Informatie Vlaanderen")
                 )
)



server <- function(input, output) {
  
  labels_a <- sprintf(
    "iNaturalist %s",
    all_spatial$inaturalist_id 
  ) %>% lapply(htmltools::HTML)
  
  pal <- 
    colorFactor(palette = c("blue", "red", "black" ), 
                levels = c("Individu", "Nest (behandeld)", 'Nest (niet behandeld)'))
  
  output$heatmap <- renderLeaflet({
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
  })
  
  output$management <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g nest(en) bestreden",
      municipalities$NAAM, municipalities$Freq
    ) %>% lapply(htmltools::HTML)
    
    bins <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    pal <- colorBin("YlOrRd", domain = municipalities$Freq, bins = bins, na.color='white')
    
    
    leaflet(municipalities)%>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Freq),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
      addPolygons(data=Brussel_shape, fillColor ='white', fillOpacity = 0.9, color='white')%>%
      addLegend(pal = pal,
                values= ~Freq,
                opacity = 0.9, 
                title =  'Aantal behandelde nesten in 2021',
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(c('0', '1', '2', '3', '4', '5', '6', '7', '8'))
                },
                position = "bottomright")%>%
      addScaleBar(position = "bottomleft")
  })
  
  output$actieve_haarden <- renderLeaflet ({
    
    labels_b <- sprintf(
      "iNaturalist %s",
      all_spatial_AH$inaturalist_id 
    ) %>% lapply(htmltools::HTML)
    
    pal <- 
      colorFactor(palette = c("blue", "red" ), 
                  levels = c("Individu", "Nest"))
    
    leaflet(all_spatial_AH)%>%
      addTiles() %>%
      addHeatmap(
        lng = ~longitude, lat = ~latitude, intensity = ~output_rel, 
        blur = 25, max = 1, radius = 20
      ) %>%
      addLegend(pal = pal,
                values= ~`Type observatie`,
                opacity = 0.7,
                position = "bottomright")%>%
      addScaleBar(position = "bottomleft")%>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       color= ~pal(all_spatial_AH$`Type observatie`),
                       opacity=0.5,
                       radius=2,
                       popup =  paste0("<a href=https://www.inaturalist.org/observations/", all_spatial_AH$inaturalist_id, ">", "iNaturalist", "</a>"))
    
  })
}

shinyApp(ui, server)