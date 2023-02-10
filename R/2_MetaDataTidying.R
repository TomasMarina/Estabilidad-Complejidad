# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn

# 2. MetaData tidying


##
# Cargar paquetes
##
packages <- c("dplyr", "igraph", "multiweb")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


##
# Ordenar metadatos ----
##

## GATEWAy ----
load("Data/gateway_list.rda")

gateway_df <- foodWebDF

# Merge dataframes and format
colnames(gateway_df)[4] <- "Longitude"
colnames(gateway_df)[5] <- "Latitude"
colnames(gateway_df)[3] <- "Network"
colnames(gateway_df)[6] <- "Ecosystem"

gateway_df["Ecosystem"][gateway_df["Ecosystem"] == "lakes" | gateway_df["Ecosystem"] == "streams"] <- "Dulceacuícola"
gateway_df["Ecosystem"][gateway_df["Ecosystem"] == "terrestrial aboveground" | gateway_df["Ecosystem"] == "terrestrial belowground"] <- "Terrestre"
gateway_df["Ecosystem"][gateway_df["Ecosystem"] == "marine"] <- "Marino"

gateway_meta <- gateway_df %>% 
  dplyr::select(Longitude, Latitude, Network, Ecosystem)
gateway_meta <- gateway_meta[order(gateway_meta$Network),]

## Multiweb ----
multiweb_meta <- multiweb::metadata %>%
  dplyr::select(c(Longitude, Latitude, Network))
multiweb_meta["Longitude"][multiweb_meta["Network"] == "Caribbean Reef"] <- "-66 39 52.2468"
multiweb_meta$Longitude = (measurements::conv_unit(multiweb_meta$Longitude, from = 'deg_min_sec', to = 'dec_deg') )
multiweb_meta$Latitude = (measurements::conv_unit(multiweb_meta$Latitude, from = 'deg_min_sec', to = 'dec_deg') )
multiweb_meta$Ecosystem <- "Marino"


all_meta <- (rbind(distinct(gateway_meta), distinct(multiweb_meta))) %>%
  mutate_at(vars(Longitude, Latitude), as.numeric)


##
# Guardar ----
##

save(gateway_meta, multiweb_meta, all_meta,
     file = "Data/metadata.rda")
