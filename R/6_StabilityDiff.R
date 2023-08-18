# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn

# 6. Stability differences btw ecosystems


##
# Cargar paquetes
##
packages <- c("dplyr","rcompanion")
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

load("Resultados/all_res.rda")
load("Data/metadata.rda")

##
# Unir resultados y metadata
##
# Descartar redes c/2 componentes de metadata
all_meta_ok <- all_meta %>% 
  filter(!Network %in% c(fw_2_disc)) %>% 
  dplyr::select(Ecosystem)
all_data <- cbind(all_res, all_meta_ok) %>% 
  dplyr::select(Network, Ecosystem, everything())


##
# Modularidad ----
##

mod_stat <- all_data %>% 
  dplyr::select(Network, Ecosystem, Modularity) %>% 
  group_by(Ecosystem) %>%
  summarise(
    count = n(),
    mean = mean(Modularity, na.rm = TRUE),
    sd = sd(Modularity, na.rm = TRUE),
    median = median(Modularity),
    IQR = IQR(Modularity, na.rm = TRUE))

# Kruskal-Wallis
KW_mod <- kruskal.test(Modularity ~ Ecosystem, data = all_data)
## Size effect ----
# Taken from https://rcompanion.org/handbook/F_04.html
# 0.01 – < 0.08 small
# 0.08 – < 0.26 medium
# ≥ 0.26 large
drop_f_m <- all_data %>% 
  mutate(across(where(is.character), factor)) %>% 
  filter(., Ecosystem != "Marino") %>% 
  droplevels()
str(drop_f_m)  # check dropped levels
epsilonSquared(x = drop_f_m$Modularity, 
               g = drop_f_m$Ecosystem)

# Multiple pairwise-comparison
pairwise.wilcox.test(all_data$Modularity, all_data$Ecosystem,
                     p.adjust.method = "bonf")


##
# QSS ----
##

qss_stat <- all_data %>% 
  dplyr::select(Network, Ecosystem, QSS_MEing) %>% 
  group_by(Ecosystem) %>%
  summarise(
    count = n(),
    mean = mean(QSS_MEing, na.rm = TRUE),
    sd = sd(QSS_MEing, na.rm = TRUE),
    median = median(QSS_MEing),
    IQR = IQR(QSS_MEing, na.rm = TRUE))

# Kruskal-Wallis
kruskal.test(QSS_MEing ~ Ecosystem, data = all_data)
## Size effect ----
# Taken from https://rcompanion.org/handbook/F_04.html
# 0.01 – < 0.08 small
# 0.08 – < 0.26 medium
# ≥ 0.26 large
drop_f_q <- all_data %>% 
  mutate(across(where(is.character), factor)) %>% 
  filter(., Ecosystem != "Marino") %>% 
  droplevels()
str(drop_f_q)  # check dropped levels
epsilonSquared(x = drop_f_q$QSS_MEing, 
               g = drop_f_q$Ecosystem)

# Multiple pairwise-comparison
pairwise.wilcox.test(all_data$QSS_MEing, all_data$Ecosystem,
                     p.adjust.method = "hommel")
