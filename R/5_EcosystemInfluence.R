# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn

# 4. Testing ecosystem influence in Modularity & QSS index
# Following https://www.andrew.cmu.edu/user/achoulde/94842/lectures/lecture10/lecture10-94842.html


##
# Cargar paquetes
##
packages <- c("dplyr", "ggplot2", "interactions", "agricolae", "emmeans")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

##
# Cargar datos
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


### Modularity ----
##### Test intercept ----
## Is a categorical variable in a regression statistically significant?

# I want to know if the ecosystem-specific intercepts capture significantly more variation in the outcome 
# (Modularity) than the single intercept model, or if allowing for different intercepts isn’t doing much more 
# than capturing random fluctuations in the data

# Model allowing different intercepts
# Model Modularity as a linear function of Connectance, allowing for 
# a ecosystem-specific intercept for each of the three ecosystem categories,
# with the same slope governing the regression
mod.lm <- lm(Modularity ~ Connectance + Ecosystem, data = all_data)
summary(mod.lm)
# 'Dulceacuícola' ecosystems (baseline level) have statistically different intercept
# than 'Terrestre', but similar to 'Marino'.

# Model simple (no different intercepts)
# Fit the simpler model with Connectance as the only predictor
mod.lm.simple <- lm(Modularity ~ Connectance, data = all_data)
summary(mod.lm.simple)

# Compare models
anova(mod.lm.simple, mod.lm)
# This output tells us that the Ecosystem variable is statistically significant: 
# It is unlikely that the improvement in fit when adding the Ecosystem variable is simply due to 
# random fluctuations in the data. Thus it is important to consider Ecosystem when 
# modeling how Modularity depends on Connectance.


##### Test slope ----
# Is an interaction term significant?

# Model Modularity as a linear function of Connectance, allowing for 
# a ecosystem-specific slope & intercept for each of the three ecosystem categories
mod.lm.interact <- lm(Modularity ~ Connectance * Ecosystem, data = all_data)
summary(mod.lm.interact)

# Do the lines with different slopes fit the data significantly better than the common slope model?
# Let’s compare the two with the anova() function
anova(mod.lm, mod.lm.interact)
# Since p-value is 0.45 there is not enough evidence to conclude that the interaction term (different slopes) 
# is providing significant additional explanatory power over the simpler Connectance + Ecosystem model

# Graph showing overlap of regression lines for Ecosystem categories
cols <- c("Dulceacuícola"="#D55E00", "Marino"="#0072B2", "Terrestre"="#009E73")
interact_plot(mod.lm.interact, pred = Connectance, modx = Ecosystem, plot.points = TRUE,
              interval = TRUE, int.width = 0.95,
              legend.main = "Ecosistema", x.label = "Complejidad", y.label = "Modularidad",
              colors = cols) + 
  theme_classic()


##### Pairwise compare ----
m <- lm(Modularity ~ Ecosystem * Connectance, all_data)
emtrends(m, pairwise ~ Ecosystem, var="Connectance", infer=c(TRUE,TRUE))
emmip(m, Ecosystem ~ Connectance, cov.reduce = range)
# No hay diferencia en las tendencias de las pendientes


### QSS ----
##### Test intercept ----

qss.lm <- lm(QSS_MEing ~ Connectance + Ecosystem, data = all_data)
summary(qss.lm)
# 'Dulceacuícola' ecosystems (baseline level) have statistically different intercept
# than 'Terrestre', but similar to 'Marino'.

# Model simple (no different intercepts)
# Fit the simpler model with Connectance as the only predictor
qss.lm.simple <- lm(QSS_MEing ~ Connectance, data = all_data)
summary(qss.lm.simple)

# Compare models
anova(qss.lm.simple, qss.lm)
# This output tells us that the Ecosystem variable is statistically significant: 
# It is unlikely that the improvement in fit when adding the Ecosystem variable is simply due to 
# random fluctuations in the data. Thus it is important to consider Ecosystem when 
# modeling how QSS depends on Connectance.


##### Test slope ----
# Is an interaction term significant?

# Model QSS as a linear function of Connectance, allowing for 
# a ecosystem-specific slope & intercept for each of the three ecosystem categories
qss.lm.interact <- lm(QSS_MEing ~ Connectance * Ecosystem, data = all_data)
summary(qss.lm.interact)

# Do the lines with different slopes fit the data significantly better than the common slope model?
anova(qss.lm, qss.lm.interact)
# Since p-value is < 0.05, the interaction term (different slopes) provides 
# significant additional explanatory power over the simpler Connectance + Ecosystem model

# Graph showing overlap of regression lines for Ecosystem categories
cols <- c("Dulceacuícola"="#D55E00", "Marino"="#0072B2", "Terrestre"="#009E73")
interact_plot(qss.lm.interact, pred = Connectance, modx = Ecosystem, plot.points = TRUE,
              interval = TRUE, int.width = 0.95,
              legend.main = "Ecosistema", x.label = "Complejidad", y.label = "Índice QSS",
              colors = cols) + 
  scale_y_reverse() +
  theme_classic()


##### Pairwise compare ----
q <- lm(QSS_MEing ~ Ecosystem * Connectance, all_data)
emtrends(q, pairwise ~ Ecosystem, var="Connectance", infer=c(TRUE,TRUE))
emmip(q, Ecosystem ~ Connectance, cov.reduce = range)
# No hay diferencia en las tendencias de las pendientes
