# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn

# 3. Linear regression analyses


##
# Cargar paquetes
##
packages <- c("dplyr", "ggplot2", "ggpubr", "grid", "olsrr")
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
# Regresión lineal ----
##

## General ----

### Modularidad ----
reg_mod_gral <- all_data %>% 
  ggplot(aes(x = Connectance, y = Modularity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Complejidad", y = "Estabilidad (Mod)") +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
reg_mod_gral

### QSS ----
reg_qss_gral <- all_data %>% 
  ggplot(aes(x = Connectance, y = QSS_MEing)) +
  geom_point() +
  scale_y_reverse() +
  geom_smooth(method = "lm") +
  labs(x = "Complejidad", y = "Estabilidad (QSS)") +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
reg_qss_gral


## Por ecosistema ----

cols <- c("Dulceacuícola"="#D55E00", "Marino"="#0072B2", "Terrestre"="#009E73")

### Modularidad ----

reg_mod_eco <- all_data %>% 
  ggplot(aes(x = Connectance, y = Modularity)) +
  geom_point(aes(color = factor(Ecosystem))) +
  scale_color_manual(values=cols) +
  facet_wrap(~ Ecosystem) +
  labs(x = "Complejidad", y = "Estabilidad (Mod)") +
  geom_smooth(method = "lm", color = "grey25") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")
reg_mod_eco

bx_mod_eco <- all_data %>% 
  ggplot(aes(x = Connectance, y = Modularity)) +
  geom_boxplot(aes(color = factor(Ecosystem), fill = factor(Ecosystem)), alpha=0.1) +
  geom_jitter(size=0.3, aes(color = factor(Ecosystem), fill = factor(Ecosystem))) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~ Ecosystem) +
  labs(x = "Complejidad", y = "Estabilidad (Mod)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")
bx_mod_eco

# Combinar gráficos
fig_mod <- ggarrange(reg_mod_eco + rremove("x.title") + rremove("y.title"), bx_mod_eco + rremove("y.title") +rremove("x.title"),
          nrow = 2)
annotate_figure(fig_mod, left = textGrob("Estabilidad (Mod)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Complejidad", gp = gpar(cex = 1.3)))

### QSS ----

reg_qss_eco <- all_data %>% 
  ggplot(aes(x = Connectance, y = QSS_MEing)) +
  geom_point(aes(color = factor(Ecosystem))) +
  scale_color_manual(values=cols) +
  scale_y_reverse() +
  facet_wrap(~ Ecosystem) +
  labs(x = "Complejidad", y = "Estabilidad (QSS)") +
  geom_smooth(method = "lm", color = "grey25") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")
reg_qss_eco

bx_qss_eco <- all_data %>% 
  ggplot(aes(x = Connectance, y = QSS_MEing)) +
  geom_boxplot(aes(color = factor(Ecosystem), fill = factor(Ecosystem)), alpha=0.1) +
  geom_jitter(size=0.3, aes(color = factor(Ecosystem), fill = factor(Ecosystem))) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_y_reverse() +
  facet_wrap(~ Ecosystem) +
  labs(x = "Complejidad", y = "Estabilidad (QSS)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")
bx_qss_eco

# Combinar gráficos
fig_qss <- ggarrange(reg_qss_eco + rremove("x.title") + rremove("y.title"), bx_qss_eco + rremove("y.title") +rremove("x.title"),
                     nrow = 2)
annotate_figure(fig_qss, left = textGrob("Estabilidad (QSS)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Complejidad", gp = gpar(cex = 1.3)))

