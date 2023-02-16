# Complejidad-Estabilidad en redes tróficas empíricas
# Autores: Tomás I. Marina & Nate Colbrunn

# 3. Linear regression analyses


##
# Cargar paquetes
##
packages <- c("dplyr", "ggplot2", "ggpubr", "grid", "olsrr", "ggpmisc")
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
  labs(x = "Complejidad", y = "Modularidad") +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
reg_mod_gral
# Ecuación regresión lineal
summary(lm(Modularity ~ Connectance, data = all_data))

#### Regresión por cuantil ----

hist(all_data$Modularity, prob=TRUE, col = "blue", border = "black")
lines(density(all_data$Modularity))

library(quantreg)
library(SparseM)
Mreg75=rq(Modularity ~ Connectance, tau=0.75, data = all_data)
summary(Mreg75)
Mreg50=rq(Modularity ~ Connectance, tau=0.50, data = all_data)
summary(Mreg50)
Mreg25=rq(Modularity ~ Connectance, tau=0.25, data = all_data)
summary(Mreg25)
anova(Mreg75, Mreg50)  # p-value = 0.017 *
anova(Mreg25, Mreg50)  # p-value = 0.103
anova(Mreg75, Mreg25)  # p-value = 0.0006 ***

reg_mod_gral + 
  geom_quantile(quantiles = 0.75) + geom_quantile(quantiles = 0.25)

### QSS ----
reg_qss_gral <- all_data %>% 
  ggplot(aes(x = Connectance, y = QSS_MEing)) +
  geom_point() +
  scale_y_reverse() +
  #geom_quantile() +
  geom_smooth(method = "lm") +
  labs(x = "Complejidad", y = "Índice QSS") +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
reg_qss_gral
# Ecuación regresión lineal
Qreg_mn <- lm(QSS_MEing ~ Connectance, data = all_data)
summary(Qreg_mn)

#### Regresión por cuantil ----

hist(all_data$QSS_MEing, prob=TRUE, col = "blue", border = "black")
lines(density(all_data$QSS_MEing))

library(quantreg)
library(SparseM)
Qreg90=rq(QSS_MEing ~ Connectance, tau=0.90, data = all_data)
summary(Qreg90)
Qreg75=rq(QSS_MEing ~ Connectance, tau=0.75, data = all_data)
summary(Qreg75)
Qreg50=rq(QSS_MEing ~ Connectance, tau=0.50, data = all_data)
summary(Qreg50)
Qreg25=rq(QSS_MEing ~ Connectance, tau=0.25, data = all_data)
summary(Qreg25)
Qreg10=rq(QSS_MEing ~ Connectance, tau=0.10, data = all_data)
summary(Qreg10)
anova(Qreg75, Qreg50)  # p-value = 0.025 *
anova(Qreg25, Qreg50)  # p-value = 0.126
anova(Qreg75, Qreg25)  # p-value = 0.00993 **

reg_qss_gral + 
  geom_quantile(quantiles = 0.75) + geom_quantile(quantiles = 0.25)


# Check cluster by QSS
library(factoextra)
library(cluster)
ggplot(all_data, aes(x = QSS_MEing)) + geom_density() + theme_bw()
ggplot(all_data, aes(x = QSS_MEing)) + geom_histogram(bins=50) + theme_bw()
# Determine and visualize the optimal number of clusters using total within sum of square
data.scaled <- scale(all_data$QSS_MEing)
fviz_nbclust(data.scaled, kmeans, method = "wss")
# Calculate gap statistic based on number of clusters
gap_stat <- clusGap(data.scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 500)
# Plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
# Perform k-means clustering with k = predicted clusters (gap_stat)
km <- kmeans(data.scaled, centers = 1, nstart = 25)
# NO CLUSTERING FOUND! #


# Combinar gráficos
fig_gral <- ggarrange(reg_mod_gral + rremove("x.title"), reg_qss_gral + rremove("x.title"),
                     ncol = 2)
annotate_figure(fig_gral, bottom = textGrob("Complejidad", gp = gpar(fontface = "bold", cex = 1.3)))


## Por ecosistema ----

cols <- c("Dulceacuícola"="#D55E00", "Marino"="#0072B2", "Terrestre"="#009E73")

### Modularidad ----

reg_mod_eco <- all_data %>% 
  ggplot(aes(x = Connectance, y = Modularity)) +
  geom_point(aes(color = factor(Ecosystem))) +
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 0, label.y = 0.5) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation
                        label.x = 0, label.y = 0.52) +
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
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 0, label.y = 0.5) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation
                        label.x = 0, label.y = 0.8) +
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

