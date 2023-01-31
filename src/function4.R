function4 <- function(Individual, Nest_all) {
  
  require(httr)
  require(sf)
  require(sp)
  require(raster)
  require(dplyr)
  require(gdalUtilities)


  Brussel_shape <- readOGR('./data/Export_Output.shp')#/Scripts/VespaWatch_app/
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  Brussel_shape<- spTransform(Brussel_shape, crs_wgs)
  
  return (Brussel_shape)
  
  }