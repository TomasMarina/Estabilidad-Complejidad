# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn

# 1. Tidy GATEWAy database


##
# Cargar paquetes
##
packages <- c("dplyr")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


##
# Cargar bases de datos ----
##

## GATEWAy ----
gateway_db <- read.csv("Data/DataBase_GATEWAy.csv", header = TRUE, na.strings="NA")

# Setup first database as dataframe and format
foodWebDF <- gateway_db %>% 
  dplyr::select(res.taxonomy, con.taxonomy, foodweb.name, longitude, latitude, ecosystem.type, study.site) %>% 
  dplyr::filter(!foodweb.name == "Carpinteria" ) %>%
  dplyr::filter(!foodweb.name == "Weddell Sea" )
foodWebDF["ecosystem.type"][foodWebDF["study.site"] == "Grand Caricaie"] <- "terrestrial aboveground"
foodWebDF["foodweb.name"][foodWebDF["foodweb.name"] == "Caribbean Reef"] <- "Caribbean Reef Small"

# Split duplicated network and create distinct food web names
carp_mar <- gateway_db %>% 
  dplyr::filter(foodweb.name == "Carpinteria" & ecosystem.type == "marine") %>% 
  dplyr::select(res.taxonomy, con.taxonomy, foodweb.name, longitude, latitude, ecosystem.type, study.site)
carp_mar$foodweb.name <- "Carpinteria Marine"

carp_ter <- gateway_db %>%
  dplyr::filter(foodweb.name == "Carpinteria" & ecosystem.type == "terrestrial aboveground") %>% 
  dplyr::select(res.taxonomy, con.taxonomy, foodweb.name, longitude, latitude, ecosystem.type, study.site)
carp_ter$foodweb.name <- "Carpinteria Terrestrial"

# Merge dataframes
foodWebDF_up <- rbind(foodWebDF, carp_mar)
foodWebDF <- rbind(foodWebDF_up, carp_ter)

# Split into lists based on food web name
datrescon <- foodWebDF %>% 
  dplyr::select(res.taxonomy, con.taxonomy, foodweb.name)

gateway_list <- split(datrescon, datrescon$foodweb.name)


##
# Guardar ----
##

save(gateway_list, foodWebDF, file = "Data/gateway_list.rda")
