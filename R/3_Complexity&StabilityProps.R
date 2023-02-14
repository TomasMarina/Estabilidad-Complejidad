# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn

# 3. Analyses complexity & stability


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
# Cargar datos ----
##

# GATEWAy
load("Data/gateway_list.rda")

glist_gateway <- mapply(graph_from_data_frame, d = gateway_list, directed = TRUE)

# Multiweb
glist_multiweb <- multiweb::netData

# Merge g lists
glist_complete <- c(glist_gateway, glist_multiweb)


##
# Complejidad ----
##
prop_comp <- calc_topological_indices(glist_complete)

## Filtrar redes ----
# Eliminar redes == 2 componentes
fw_2_comp <- prop_comp %>% 
  filter(., Components == 2) %>% 
  dplyr::select(Network, Components)
fw_2_disc <- pull(fw_2_comp, Network)
# Nueva lista con redes de 1 componente
glist_complete_ok <- within(glist_complete, rm(list=fw_2_disc))

# Calcular complejidad lista depurada (1 comp)
prop_comp_ok <- calc_topological_indices(glist_complete_ok)


##
# Estabilidad ----
##

## Quasi-Sign Stability ----
QSS_res <- calc_QSS(glist_complete_ok, nsim = 1000, ncores = 4)
QSS_res <- QSS_res %>% 
  rename(QSS_prop = QSS, QSS_MEing = MEing) 

## Modularidad ----
mod_res <- calc_modularity(glist_complete_ok, weights = NA, ncores = 4)


##
# Complejidad-Estabilidad ----
##

all_res <- cbind(prop_comp_ok, mod_res, QSS_res)


##
# Guardar ----
##

save(prop_comp, prop_comp_ok, QSS_res, mod_res, all_res, fw_2_disc,
     file = "Resultados/all_res.rda")
