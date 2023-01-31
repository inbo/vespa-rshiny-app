library(googledrive)

#PART1: read in data
Nest <- read.csv("https://www.vespawatch.be/api/csv_export/vv_confirmed_nests/")
Management <- read.csv("https://www.vespawatch.be/api/csv_export/management_actions")
Individual <- read.csv("https://www.vespawatch.be/api/csv_export/vv_confirmed_individuals/")
names(Individual) <- c('id', 'observation_time', 'species', 'latitude', 'longitude', 'originates_in_vespawatch', 'inaturalist_id', 'individual_count', 'behaviour')

write.csv(Individual,file = 'Individual.csv')
write.csv(Nest,file = 'Nest.csv')
write.csv(Management,file = 'Management.csv')

token_folder <- "16JEzFvr85ABBTbkJNGFoqNU7sKww9Ky_"
naam_bestand <- 'Individual.csv'

save_googledrive_csv <- function(token_folder,
                                     naam_bestand) {
  drive_folder <- token_folder
  
  drive_auth(email='jasmijn.hillaert@inbo.be')
  
  drive_put(naam_bestand, path = as_id(drive_folder))
}

save_googledrive_csv(token_folder, naam_bestand)