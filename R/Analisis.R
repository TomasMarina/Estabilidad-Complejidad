# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn


##
# Cargar paquetes
##
packages <- c("igraph", "multiweb", "dplyr")
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
load("Data/igcomplete.rda")
iglist <- igcomplete

##
# Complejidad ----
##
prop_comp <- calc_topological_indices(iglist)

## Filtrar redes ----
# Eliminar redes == 2 componentes
fw_2_comp <- prop_comp %>% 
  filter(., Components == 2) %>% 
  select(Network, Components)
fw_2_disc <- pull(fw_2_comp, Network)
# Nueva lista con redes de 1 componente
iglist_ok <- within(iglist, rm(list=fw_2_disc))

# Calcular complejidad lista depurada (1 comp)
prop_comp_ok <- calc_topological_indices(iglist_ok)


##
# Estabilidad ----
##

## Quasi-Sign Stability ----
QSS_res <- calc_QSS(iglist_ok, nsim = 1000, ncores = 4)

## Modularidad ----
mod_res <- calc_modularity(iglist_ok, weights = NA, ncores = 4)

