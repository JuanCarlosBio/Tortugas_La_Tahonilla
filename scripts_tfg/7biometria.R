################################################################################
#############################      Biometría     ###############################
################################################################################

biometria <- read_excel("biometria_tfg.xlsx", 
                        range = "A3:J891", col_types = c("text", 
                                                         "text", "text", "text", "text", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric")) %>% 
  select(fecha=Fecha, especie=Especie, lrc = `L. Recto`, lcc = `L. Curvo`,
         arc = `A. Recto`, acc = `A. Curvo`, peso = Peso)

biometria$fecha <- strptime(as.character(biometria$fecha), "%d/%m/%Y")

nueva_fecha <- format(biometria$fecha, "%Y-%m-%d")

biometria <- biometria %>%
mutate(anio=year(fecha),
       mes=month(fecha, label = T),
       estacion=case_when(mes == "dic" | mes == "ene" | mes == "feb"~"Winter",
                            mes == "mar" | mes == "abr" | mes == "may"~"Spring",
                            mes == "jun" | mes == "jul" | mes == "ago"~"Summer",
                            mes == "sep" | mes == "oct" | mes == "nov"~"Otoño")) %>% 
  filter(lrc > 0,lcc > 0,
         acc > 0,arc > 0, arc < 2000,
         peso > 0,peso < 250) %>% 
  mutate(estadio=case_when(lcc<20 ~ "Hatchling",
                           lcc>=20 & lcc<40~ "Small.Juv",
                           lcc>=40 & lcc<60~"Big.Juv",
                           lcc>=60 & lcc<80 ~"Subadult",
                           lcc>=80 ~"Adult"))

 # biometria %>%
 #   group_by(especie,estadio) %>%
 #   summarise(n=n())
 #   summarise(n=n(),
 #            media_lrc=mean(lrc),
 #            media_lcc=mean(lcc),
 #            media_arc=mean(arc),
 #            media_acc=mean(acc),
 #            media_peso=mean(peso))

#------------------------------------------------------------------------------#
#                               Caretta caretta                                #
#------------------------------------------------------------------------------#

biomet.cc <- biometria %>% filter(especie %in% "Caretta caretta") # n = 671


# #------------------------------------------------------------------------------#
# #                 Estudio de la longitud del recto del caparazón               #
# #------------------------------------------------------------------------------#


estudio_l_c <- data.frame(
  valores=c(27,380,227,33,4),
  tipo=c("Hatchling", "Small juvenile",
         "Big juvenile","subadult",
         "Adult")
) %>% mutate(tipo=factor(tipo,
                         levels = c("Hatchling", "Small juvenile",
                                    "Big juvenile","subadult",
                                    "Adult")))

cc <- estudio_l_c %>%
  ggplot(aes(tipo, valores)) +
  geom_bar(stat = "identity", fill="orange",
           col="black", width = .65) +
  geom_text(data = tibble(x=c(1,2,3,4,5), y=estudio_l_c$valores+45),
            aes(x=x,y=y), label=c("27","380","227","33","4"),
            inherit.aes = F, size=4) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,550))+
  scale_x_discrete(breaks=c("Hatchling", "Small juvenile",
                            "Big juvenile","subadult",
                            "Adult"),
                   labels=c("(<20cm)\nHatchling","(20-40cm)\nSmall\njuvenile",
                            "(40-60cm)\nBig\njuvenile","(60-80cm)\nSubadult",
                            "(>80cm)\nAdult")) +
  labs(title = "Curved caparace length, *C.caretta*",
       subtitle="n = (671)",
       x="CCL (cm)",
       y="Num.turtles") +
  theme_pander() +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "lightblue4"),
    plot.title = element_markdown(hjust = .5),
    plot.subtitle = element_markdown(hjust = .5),
    axis.line.x = element_line(),
    axis.title.x = element_text(face = "bold",
                                margin = margin(t=10)),
    axis.title.y = element_text(face = "bold",
                                margin = margin(r=10)));cc

# 
# #------------------------------------------------------------------------------#
# #                              Chelonia mydas                                  #
# #------------------------------------------------------------------------------#
# 
biometcm <- biometria %>% filter(especie %in% "Chelonia mydas")
        # n = 18


 
# #------------------------------------------------------------------------------#
# #                 Estudio de la longitud del recto del caparazón               #
# #------------------------------------------------------------------------------#


estudio_l_cm <- data.frame(
  valores=c(0,5,6,7,0),
  tipo=c("Hatchling", "Small juvenile",
         "Big juvenile","subadult",
         "Adult")
) %>% mutate(tipo=factor(tipo,
                         levels = c("Hatchling", "Small juvenile",
                                    "Big juvenile","subadult",
                                    "Adult")))

cm <- estudio_l_cm %>%
  ggplot(aes(tipo, valores)) +
  geom_bar(stat = "identity", fill="forestgreen",
           col="black", width = .65) +
  geom_text(data = tibble(x=c(1,2,3,4,5), y=estudio_l_cm$valores+2),
            aes(x=x,y=y), label=c("","5","6","7",""), inherit.aes = F, size=4) +
   scale_y_continuous(expand = expansion(0),
                      limits = c(0,15))+
  scale_x_discrete(breaks=c("Hatchling", "Small juvenile",
                            "Big juvenile","subadult",
                            "Adult"),
                   labels=c("(<20cm)\nHatchling","(20-40cm)\nSmall\njuvenile",
                            "(40-60cm)\nBig\njuvenile","(60-80cm)\nSubadult",
                            "(>80cm)\nAdult")) +
  labs(title = "Curved caparace lenght, *C.mydas*",
       subtitle="n = (18)",
       x="CCL (cm)",
       y="Num.turtles") +
  theme_pander() +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "lightblue4"),
    plot.title = element_markdown(hjust = .5),
    plot.subtitle = element_markdown(hjust = .5),
    axis.line.x = element_line(),
    axis.title.x = element_text(face = "bold",
                                margin = margin(t=10)),
    axis.title.y = element_text(face = "bold",
                                margin = margin(r=10))
  );cm

plot_grid(cc,cm)

# ggsave("biometria1.png", path="C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\Tortugas_La_Tahonilla\\graficas",
#       width = 9,
#       height = 4)


#------------------------------------------------------------------------------#
#                                 PCA                                          #
#------------------------------------------------------------------------------#


variables <- biomet.cc[,c("lrc","lcc","arc","acc","peso")] 

pca <- prcomp(as.matrix(variables),center = T, scale = T)
sumatorio_pca<-summary(pca)

df_sumatorio <- sumatorio_pca$importance

var_pc1 <- round(df_sumatorio[2,1]*100, 2)
var_pc2 <- round(df_sumatorio[2,2]*100, 2)

plot(pca, main = "PCA",
     xlab="Componentes")

biplot(pca, scale = 0)

plot(pca, main = "PCA",
     xlab="Componentes")

biometria_es <- biomet.cc[,c("especie","estacion","estadio")] 

pca_biomet <- data.frame(biometria_es,
                         pca$x[,c("PC1","PC2","PC3")]) %>% view()
#############################
# Correlación PC, variables #
#############################

cor_pca <- cor(biomet.cc[,c("lrc","lcc","arc","acc","peso")],
    pca_biomet[,c("PC1","PC2")]);cor_pca

write.xlsx(cor_pca, 'pca_correlacion.xlsx')
#### PCA ESPECIES
colnames(pca_biomet)
pca_biomet %>% 
  filter(especie %in% "Caretta caretta") %>%
  mutate(estadio=factor(estadio,
                        levels = c("Hatchling","Small.Juv",
                                   "Big.Juv","Subadult",
                                   "Adult"))) %>%
  ggplot(aes(PC1,PC2, 
             col = estadio, 
             fill=estadio,
             #alpha=especie
             )) +
  geom_point() +
  theme_classic() +
  labs(x=glue("PC1 ({var_pc1}% explained variance)"),
       y=glue("PC2 ({var_pc2}% explained variance)"),
       fill=NULL,
       color=NULL,
       title = glue("PCA Biometric variables,<br><span style='color: red'>cumulative variance explained: {var_pc1+var_pc2}%</span>")) +
  stat_ellipse(geom = "polygon", alpha=.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("magenta", "gray","forestgreen",
                                "blue","orange")) +
  scale_fill_manual(values = c("magenta", "gray","forestgreen",
                               "blue","orange")) +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "lightblue4"),
    plot.title = element_markdown(face="bold", hjust = .5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = c(.6,.3),
    legend.background = element_rect(color = "black", fill = NA),
    legend.margin = margin(t = -.1, r = .1, b = .1, l = .1, unit = "cm")
  )

ggsave("biometria2.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\Tortugas_La_Tahonilla\\graficas",
       width = 6, height = 5)


##############
# Gráfico 3d #
##############
library(rgl)
biomet_3d <- as_tibble(pca_biomet) %>% 
  mutate(color=case_when(estadio=="Hatchling"~"red",
                         estadio=="Small.Juv"~"gray",
                         estadio=="Big.Juv"~"forestgreen",
                         estadio=="Subadult"~"blue",
                         estadio=="Adult"~"orange",
                         TRUE~NA_character_)) 

plot3d(x=pca_biomet$PC1,y=pca_biomet$PC2,z=pca_biomet$PC3,
       col = biomet_3d$color, type = "s", size = 1,
       xlab = "PC1 (~94% variance explined)", ylab="PC2 (~4% variance explined)", zlab="PC3 (~1% variance explained)")



#################################
# Son significativos los grupo? #
#################################
library(nortest)
library(coin)
tapply(pca_biomet$PC1, pca_biomet$estadio, shapiro.test)
kruskal.test(PC1~as.factor(estadio), data = pca_biomet)               # PC1 P<0.05

d_t.1 <- dunnTest(PC1~as.factor(estadio), data = pca_biomet,
         method = "bonf");d_t.1$dtres
                                    # Adulto p>0.05 -> Juv.grande y Subadulto; p<0.05 Cría, Juv.pequeño
                                    # Cría   p>0.05 -> ninguno ;    p<0.05 Juv.grande, peq, subadulto
                                    # Juv.grande p>0.05 ninguno;    p<0.05 Juv.peq, subadulto
                                    # Juv.peq p>0.05 ninguno;       p<0.05 subadulto

write.xlsx(d_t.1$dtres, "tabla_pca1.xlsx")

tapply(pca_biomet$PC2, pca_biomet$estadio, shapiro.test)
kruskal.test(PC2~as.factor(estadio), data = pca_biomet)              # PC2 P<0.05

d_t.2 <- dunnTest(PC2~as.factor(estadio), data = pca_biomet,
                  method = "bonf");d_t.2$dtres
                                    
                                    # Adulto p>0.05 -> Cría, juv. peq, Subadulto; p<0.05 Juv.grande 
                                    # Cría p>0.05 subadulto; p<0.05 juv.peq y grande
                                    # Juv.grande p>0.05 ninguno; p<0.05 Juv.peq; 

##### PCA DE LAS ESTACIONES
pca_biomet %>% 
  filter(especie %in% "Caretta caretta") %>% 
  ggplot(aes(PC1,PC2, col = estacion, fill=estacion)) +
  geom_point() +
  theme_test() +
  labs(title = "PCA parámetros biométricos-estación",
       x="PC1 (94%)",
       y="PC2 (0.4%)",
       fill="Estación",
       color="Estación") +
  stat_ellipse(geom = "polygon", alpha=.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("cyan", "orange","yellowgreen","magenta")) +
  scale_fill_manual(values = c("cyan", "orange","yellowgreen","magenta")) +
  theme(
    plot.title = element_text(face="bold", hjust = .5),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = c(.5,.2)
  )
###############################
#       Gráfico 3D estaciones #
###############################

biomet_3d.2 <- pca_biomet %>% 
  mutate(color.2=case_when(estacion=="Primavera"~"yellowgreen",
                           estacion=="Verano"~"magenta",
                           estacion=="Otoño"~"orange",
                           estacion=="Invierno"~"cyan",
                         TRUE~NA_character_)) %>% view()

plot3d(x=pca_biomet$PC1,y=pca_biomet$PC2,z=pca_biomet$PC3,
       col = biomet_3d.2$color, type = "s", size = 1,
       xlab = "PC1 (94%)", ylab="PC2 (0.4%)", zlab="PC3 (0.01)")

#################################
# Son significativos los grupo? #
#################################

tapply(pca_biomet$PC1, pca_biomet$estacion, lillie.test)
kruskal.test(PC1~as.factor(estacion), data = pca_biomet)               
d_te1 <- dunnTest(PC1~as.factor(estacion), data = pca_biomet,
         method = "bonf");d_te1$dtres

tapply(pca_biomet$PC2, pca_biomet$estacion, lillie.test)
kruskal.test(PC2~as.factor(estacion), data = pca_biomet)               

cor()


library(psych)
pairs.panels(biomet.cc[,c("lrc","lcc","arc","acc","peso")],
             main = "Correlacion lineal tortuga boba")


rotados <- principal(variables, rotate = "varimax", nfactors = 2, scores = TRUE)

cr_biometria <- as_tibble(rotados$scores[,c(1,2)]) %>% 
   mutate(estadio = biomet.cc$estadio,
          estadio=factor(estadio,
                        levels = c("Hatchling","Small.Juv",
                                   "Big.Juv","Subadult",
                                   "Adult"))) 

cr_biometria %>%
  ggplot(aes(RC1, RC2, fill=estadio, color=estadio)) +
  geom_point() +
  theme_classic() +
  labs(x=glue("PC1 ({var_pc1}% explained variance)"),
       y=glue("PC2 ({var_pc2}% explained variance)"),
       fill=NULL,
       color=NULL,
       title = "PCA Biometric variables") +
  stat_ellipse(geom = "polygon", alpha=.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("red", "gray","forestgreen",
                                "blue","orange")) +
  scale_fill_manual(values = c("red", "gray","forestgreen",
                               "blue","orange")) +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "lightblue4"),
    plot.title = element_text(face="bold", hjust = .5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "top",
    legend.background = element_rect(color = "black", fill = NA),
    legend.margin = margin(t = -.1, r = .1, b = .1, l = .1, unit = "cm")
  )

