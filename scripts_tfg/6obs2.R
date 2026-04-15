################################################################################
############################## Observaciones 2 #################################
################################################################################

source("1Lib_DFs.R")

selec <-data_tortugas_tfg.xlsxselec <-function(ord,lista_tokens,var) {
  paste(lista_tokens[-ord],collapse="|")
  if(!is.na(ord)) return(grepl(lista_tokens[ord],tolower(var)) & !grepl(paste(lista_tokens[-ord],collapse="|"),tolower(var)))
  else return(grepl(paste(lista_tokens,collapse="|"),tolower(var)))
}

# Lista de las lesiones de la tortuga
lista_lesiones<-c("amput|amputa|amputada","corte","enmalla|enreda|enmallamiento|
                  rafia|red|nylon|malla","anzuelo|anz","fractura","parás|paras|gusan",
                  "ahoga","petrol|hidrocarburo","herida|golpe|heridas|embarcaci","Apn|apn|APN")

#                  "herida","enredada","inflama"
#                  "golpe","infección","mordida","petrol","parásitos",
#                  "gangrenada","hinchada","necrosado","ahoga","lesiones")
colnames(df_tortugas_marinas)

# Lista del estado en el que se encuentra la tortuga

lista_estado<-c("muer|aplas|morib","déb|deb","necrosada|gangrena","flaca|delgada",
                "infec|inflam|hincha|inflamada","putre|podri|descompuesta|descomposici","Apn|apn|APN")


# Lista de las partes del cuerpo afectada por la tortuga

lista_cuerpo<-c("aleta|delantera|izda|derecha|traseras|dcha","caparaz|caparazón|caparazon|cuerpo",
                "ojo|ojos","boca|pico","cabeza","cuello","Apn|apn|APN")


df_tortugas_obs <- df_tortugas_marinas %>% mutate(
  lesion= case_when(selec(1,lista_lesiones,observa)~"Amputación" ,
                     selec(2,lista_lesiones,observa)~"Cortes",
                     selec(3,lista_lesiones,observa)~"Enredadas",
                     selec(4,lista_lesiones,observa)~"Anzuelos",
                     selec(5,lista_lesiones,observa)~"Fracturas",
                     selec(6,lista_lesiones,observa)~"Parásitos",
                     selec(7,lista_lesiones,observa)~"Ahogadas",
                     selec(8,lista_lesiones,observa)~"Petroleadas",
                     selec(9,lista_lesiones,observa)~"Herida",
                     selec(10,lista_lesiones,observa)~"Nada",
                     selec(NA,lista_lesiones,observa) ~"Varias lesiones"),
  estado= case_when(selec(1,lista_estado,observa) ~"Muerta" ,
                    selec(2,lista_estado,observa)~"Débil",
                    selec(3,lista_estado,observa)~"Necrosada",
                    selec(4,lista_estado,observa)~"Flaca",
                    selec(5,lista_estado,observa)~"Infección",
                    selec(6,lista_estado,observa)~"Putrefacta",
                    selec(7,lista_estado,observa)~"Nada"
  ),
  cuerpo= case_when(selec(1,lista_cuerpo,observa) ~"Aleta" ,
                     selec(2,lista_cuerpo,observa)~"Caparazón",
                     selec(3,lista_cuerpo,observa)~"Ojos",
                     selec(4,lista_cuerpo,observa)~"Boca",
                     selec(5,lista_cuerpo,observa)~"Cabeza",
                     selec(6,lista_cuerpo,observa)~"Cuello",
                    selec(7,lista_cuerpo,observa)~"Nada",
                     selec(NA,lista_cuerpo,observa) ~"Varias")
) %>% as.data.frame() 


###############################################################################
#### Variables de Lesión-Cuerpo-Estado de las tortugas llegadas al CRFS  ######
###############################################################################


lesion <- expand.grid(lesion=as.character(unique(as.character(df_tortugas_obs$lesion)))) %>% left_join(
  df_tortugas_obs %>% group_by(lesion) %>% summarize(n=n()),
  by="lesion"
) %>% mutate(n=ifelse(is.na(n),0,n)) 

 # cuerpo %>% arrange(n) %>% 
 #   mutate(x=c(1,2,3,4,5,6,7,8,9,10,11),
 #          y=n+100,
 #          n_character=as.character(n)) %>% 
 #   filter(!(n_character %in% "1045")) %>% 
 #   summarise(suma=sum(n))

# Etiquetas de lesiones
etiquetas_lesiones <- lesion %>% arrange(n) %>% 
  mutate(x=c(1,2,3,4,5,6,7,8,9,10,11,12),
         y=n+100,
         n_character=as.character(n));etiquetas_lesiones

graf_lesion <- lesion %>% 
  #filter(lesion != "NA") %>% 
  ggplot(aes(reorder(lesion,n),n, fill = lesion)) +
  geom_bar(stat = "identity", show.legend = F,
           width = .75, col = "black") +
  geom_text(data = etiquetas_lesiones,
            aes(x=x,y=y,label=n_character),inherit.aes = F) +
  scale_x_discrete(breaks = c(NA,"Varias lesiones","Enredadas","Cortes","Amputación","Anzuelos",
                              "Herida","Nada","Fracturas","Petroleadas","Parásitos","Ahogadas"),
                   labels = c("Unclassified","Many injuries","Entangled","Cuts","Amputation","Hooks",
                              "Wound","Nothing","Fractures","Oiled","Parasites","Drowned")) +
  scale_fill_manual(breaks = c(NA,"Varias lesiones","Enredadas","Cortes","Amputación","Anzuelos",
                               "Herida","Nada","Fracturas","Petroleadas","Parásitos","Ahogadas"),
                    values = c("lightslategray","navajowhite1","salmon4","cadetblue2","grey28","goldenrod2",    
                               "gray93","olivedrab3","brown2","pink4","grey20","gray60","white","blue")) +
  coord_flip() +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1300),
                     breaks = seq(0,1100,200)) +
  labs(x = "Injury",
       y = "Num.turtles",
       title = "List of turtle injuries") +
  theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
    #   panel.grid = element_blank(),
        axis.line.x = element_line(),
        title = element_markdown(size = 10, face = "bold"),
        #axis.ticks.x = element_blank(),
        axis.text = element_text(size=10),
        axis.text.x = element_markdown(),
        axis.text.y = element_markdown(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10),size = 12),
        axis.title.y = element_text(margin = margin(r = 10),size = 12),
        plot.caption =  element_markdown(hjust = 0, face = "italic"));graf_lesion


cuerpo <- expand.grid(cuerpo=as.character(unique(as.character(df_tortugas_obs$cuerpo)))) %>% left_join(
  df_tortugas_obs %>% group_by(cuerpo) %>% summarize(n=n()),
  by="cuerpo"
) %>% mutate(n=ifelse(is.na(n),0,n)) 


# Etiquetas del cuerpo
etiquetas_cuerpo <- cuerpo %>% arrange(n) %>% 
  mutate(x=c(1,2,3,4,5,6,7,8,9),
         y=n+90,
         n_character=as.character(n));etiquetas_cuerpo

graf_cuerpo <- cuerpo %>% 
  #filter(cuerpo != "NA") %>% 
  ggplot(aes(reorder(cuerpo,n),n, fill = cuerpo)) +
  geom_bar(stat = "identity", show.legend = F,
           col = "black", width = .75) +
  geom_text(data = etiquetas_cuerpo,
            aes(x=x,y=y,label=n_character),inherit.aes = F) +
  scale_x_discrete(breaks=c(NA,"Aleta","Varias","Caparazón",
                            "Boca","Cuello","Ojos","Nada","Cabeza"),
                   labels=c("Unclassified ","Fin","Many","Carapace",
                            "Mouth","Neck","Esyes","Nothing","Head")) +
  scale_fill_manual(breaks = c("Aleta","Varias","Caparazón","Boca",
                               "cuello","Ojos","Cabeza"),
                    values = c("lightslategray","navajowhite1","salmon4","cadetblue2","grey28","goldenrod2",    
                               "gray93","olivedrab3")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1150),
                     breaks = seq(0,900,150)) +
  coord_flip() +
  labs(y = "Num.turtles",
       x = "Part of the body",
       title = "Part of the body affected list") +
  theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.x = element_line(),
        title = element_markdown(size = 10, face = "bold"),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_markdown(),
        axis.text = element_text(size=10),
        axis.text.y = element_markdown(hjust = .5),
        axis.title.x = element_text(margin = margin(t = 10),size = 12),
        axis.title.y = element_text(margin = margin(r = 10),size = 12),
        plot.caption =  element_markdown(hjust = 0, face = "italic"));graf_cuerpo

estado <- expand.grid(estado=as.character(unique(as.character(df_tortugas_obs$estado)))) %>% left_join(
  df_tortugas_obs %>% group_by(estado) %>% summarize(n=n()),
  by="estado"
) %>% mutate(n=ifelse(is.na(n),0,n))

# Etiquetas del estado
etiquetas_estado <- estado %>% arrange(n) %>% 
  mutate(x=c(1,2,3,4,5,6,7,8),
         y=n+150,
         n_character=as.character(n));etiquetas_estado

graf_estado <- estado %>% 
  #filter(estado != "NA") %>% 
  ggplot(aes(reorder(estado,n),n, fill = estado)) +
  geom_bar(stat = "identity", show.legend = F, col = "black",
           width = .75) +
  geom_text(data = etiquetas_estado,
            aes(x=x,y=y,label=n_character),inherit.aes = F) +
  coord_flip() +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1900),
                     breaks = seq(0,1600,300)) +
  scale_fill_manual(breaks = c(NA,"Necrosada","Infección","Flaca","Muerta","Nada",
                               "Débil","Putrefacta"),
                    values = c("lightslategray","navajowhite1","salmon4","cadetblue2","grey28","goldenrod2",    
                               "gray93","olivedrab3","brown2","pink4","grey20","gray60","blue")) +
  scale_x_discrete(breaks=c(NA,"Necrosada","Infección","Flaca","Muerta","Nada",
                            "Débil","Putrefacta"),
                   labels=c("Unclassified ","Necrotic","Infection","Skinny","Dead","Nothing",
                            "Weak","Putrefacted")) +
  labs(x = "State",
       y = "Num.Turtles",
       title = "State of the turtle list") +
  theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.x = element_line(),
        title = element_markdown(size = 10, face = "bold"),
        #axis.ticks.x = element_blank(),
        axis.text = element_text(size=10),
        axis.text.x = element_markdown(),
        axis.title.x = element_text(margin = margin(t = 10),size = 12),
        axis.title.y = element_text(margin = margin(r = 10),size = 12),
        plot.caption =  element_markdown(hjust = 0, face = "italic"));graf_estado

#---------------------------#
# Juntar todos las gráficos #
#---------------------------#

# plot_grid(graf_cuerpo,"",graf_estado,graf_lesion,
#           rel_heights = 1,
#           labels = c("A","","B","C"))

columna_1 <- plot_grid(graf_cuerpo,graf_estado,
                       ncol = 1, nrow = 2,
                       labels = c("A","B"))
columna_2 <- plot_grid(graf_lesion,"",
                       ncol = 1,nrow = 2,
                       rel_heights = c(1,.25),
                       labels = c("C",""))

plot_grid(columna_1,columna_2)

ggsave("obs1.png", path = "C:\\Users\\jcge9\\Desktop\\TFG\\Tortugas_La_Tahonilla\\graficas",
         width = 7.5, height = 5)

#################################################
#### Compbinación entre lesion-cuerpo-estado ####
#################################################

# Este análisis está pensado para, comparar las combinaciones de las afecciones de las
# tortugas, siempre y cuando, ambas tengas las dos varibles afectadas de alguna forma.


# Lesión-Cuerpo
lesion.cuerpo <- expand.grid(lesion=as.character(unique(as.character(df_tortugas_obs$lesion))),
                             cuerpo=as.character(unique(as.character(df_tortugas_obs$cuerpo)))) %>% left_join(
                               df_tortugas_obs %>% group_by(lesion,cuerpo) %>% summarize(n=n()),
                               by=c("lesion","cuerpo")
                             ) %>% mutate(n=ifelse(is.na(n),0,n)) 


map_calor1 <- lesion.cuerpo %>% 
  filter(!(lesion %in% NA) &  !(cuerpo %in% NA)) %>% 
  ggplot(aes(lesion, cuerpo, fill = n)) +
  geom_tile(col = "black", alpha = .9) +
  labs(x = "Injury",
       y = "Body",
       title = "Injury vs body",
       caption = "") +
  scale_x_discrete(breaks=c("Ahogadas","Amputación","Anzuelos","Cortes","Enredadas","Fracturas",
                            "Herida","Nada","Parásitos","Petroleadas","Varias lesiones"),
                   label=c("Drowned","Amputation","Hooks","Cuts","Entangled","Fractures",
                           "Wound","Nothing","Parasites","Oiled","Many")) +
  scale_y_discrete(breaks=c("Varias","Ojos","Nada","Cuello",
                            "Caparazón","Cabeza","Boca","Aleta"),
                   label=c("Many","Eyes","Nothing","Neck",
                           "Carapace","Head","Mouth","Fin")) +
  scale_fill_gradient(name="Nº tort.",
                      low = "skyblue", high = "red") +
  theme(
    panel.background = element_blank(),
    # panel.background = element_rect(fill = "azure", color = "azure"),
    #     plot.background = element_rect(fill = "azure", color = "azure"),
    #     panel.grid = element_blank(),
        #legend.background = element_rect(fill = "azure"),
       axis.line.x = element_line(),
        title = element_markdown(size = 11, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10),size = 13),
        axis.title.y = element_text(margin = margin(r = 10), size = 13),
        axis.text = element_text(size=10),
        axis.text.x = element_text(angle = 90,vjust = .16),
        plot.caption =  element_markdown(hjust = 0, face = "italic"),
        legend.title = element_text(hjust = -1.5),
        legend.key.size = unit(.45, "cm"),
        legend.text = element_text(size = 7));map_calor1

# Lesion-estado

lesion.estado <- expand.grid(lesion=as.character(unique(as.character(df_tortugas_obs$lesion))),
                             estado=as.character(unique(as.character(df_tortugas_obs$estado)))) %>% left_join(
                               df_tortugas_obs %>% group_by(lesion,estado) %>% summarize(n=n()),
                               by=c("lesion","estado")
                             ) %>% mutate(n=ifelse(is.na(n),0,n)) 


map_calor2 <-  lesion.estado %>% 
  filter(!(lesion %in% NA) &  !(estado %in% NA)) %>% 
   # group_by(lesion) %>% 
   # summarise(n=n())
  ggplot(aes(lesion, estado, fill = n)) +
  geom_tile(col = "black", alpha = .9) +
  labs(x = "Injury",
       y = "State",
       title = "Injury vs State",
       caption = "") +
  scale_x_discrete(breaks=c("Ahogadas","Amputación","Anzuelos","Cortes","Enredadas","Fracturas",
                            "Herida","Nada","Parásitos","Petroleadas","Varias lesiones"),
                   label=c("Drowned","Amputation","Hooks","Cuts","Entangled","Fractures",
                           "Wounds","NOthing","Parasites","Oiled","Many")) +
  scale_y_discrete(breaks = c("Putrefacta", "Necrosada", "Nada", "Muerta", "Infección",
                                "Flaca", "Débil"),
                     labels = c("Putrefacted","Necrotic","Nothing","Dead","Infection",
                                "Skinny","Weak")) +
  scale_fill_gradient(name="Nº tort.",
                      low = "skyblue", high = "red") +
  theme(
    # panel.background = element_rect(fill = "azure", color = "azure"),
    #     plot.background = element_rect(fill = "azure", color = "azure"),
    #     panel.grid = element_blank(),
    #     legend.background = element_rect(fill = "azure"),
    panel.background = element_blank(),
        axis.line.x = element_line(),
        title = element_markdown(size = 11, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10),size = 13),
        axis.title.y = element_text(margin = margin(r = 10), size = 13),
        axis.text = element_text(size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.185),
        plot.caption =  element_markdown(hjust = 0, face = "italic"),
        legend.title = element_text(hjust = -1.5),
        legend.key.size = unit(.45, "cm"),
        legend.text = element_text(size = 7));map_calor2

estado.cuerpo <- expand.grid(estado=as.character(unique(as.character(df_tortugas_obs$estado))),
                             cuerpo=as.character(unique(as.character(df_tortugas_obs$cuerpo)))) %>% left_join(
                               df_tortugas_obs %>% group_by(estado,cuerpo) %>% summarize(n=n()),
                               by=c("estado","cuerpo")
                             ) %>% mutate(n=ifelse(is.na(n),0,n)) 


map_calor3 <- estado.cuerpo %>% 
  filter(!(estado %in% NA) &  !(cuerpo %in% NA)) %>% 
  ggplot(aes(estado, cuerpo, fill = n)) +
  geom_tile(col = "black", alpha = .9) +
  labs(x = "State",
       y = "Body",
       title = "State vs Body") +
  scale_x_discrete(breaks = c("Débil", "Flaca", "Infección", "Muerta", "Nada", "Necrosada", "Putrefacta"),
                   labels = c("Weak", "Skinny", "Infection", "Dead", "Nothing","Necrotic", "Putrefacted")) +
  scale_y_discrete(breaks = c("Varias", "Ojos", "Nada", "Cuello", "Caparazón", "Cabeza", "Boca", "Aleta"),
                     labels = c("Many", "Eyes", "Nothing", "Neck", "Carapace", "Head", "Mouth", "Fin")) +
  scale_fill_gradient(name="Nº tort.",
                      low = "skyblue", high = "red") +
  theme(
    # panel.background = element_rect(fill = "azure", color = "azure"),
    #     plot.background = element_rect(fill = "azure", color = "azure"),
    #     panel.grid = element_blank(),
    #     legend.background = element_rect(fill = "azure"),
    panel.background = element_blank(),
        axis.line.x = element_line(),
        title = element_markdown(size = 11, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10),size = 13),
        axis.title.y = element_text(margin = margin(r = 10), size = 13),
        axis.text.x = element_text(angle = 90,vjust = 0.175),
        axis.text = element_text(size=10),
        plot.caption =  element_markdown(hjust = 0, face = "italic"),
        legend.title = element_text(hjust = -1.5),
        legend.key.size = unit(.45, "cm"),
        legend.text = element_text(size = 7));map_calor3

plot_grid(map_calor3,map_calor2,map_calor1,"",
          labels = c("A","B","C","D"))

fila_1 <- plot_grid(map_calor3,map_calor2,
                    ncol = 2, nrow = 1,
                    labels = c("A","C"),
                    rel_widths = c(.5,.6));fila_1
fila_2 <- plot_grid(map_calor1,"C",
                    ncol = 2, nrow = 1,
                    labels = c("B",""),
                    rel_widths = c(1,.250));fila_2
plot_grid(fila_1,fila_2,
          ncol = 1, nrow = 2)

# ggsave("obs2.png", path = "C:\\Users\\jcge9\\Desktop\\TFG\\Tortugas_La_Tahonilla\\graficas",
#         width = 9, height = 6)


################################################################################
#                     Comparación de las causas observaciones                  #
################################################################################



causa.cuerpo <- expand.grid(causa_orig=as.character(unique(as.character(df_tortugas_marinas$causa_orig))),
                            lesion=as.character(unique(as.character(df_tortugas_marinas$lesion))),
                            anio=as.character(unique(as.character(df_tortugas_marinas$anio)))) %>% left_join(
                               df_tortugas_marinas %>% group_by(causa_orig,lesion, anio) %>% summarize(n=n()),
                               by=c("causa_orig","lesion", "anio")
                             ) %>% mutate(n=ifelse(is.na(n),0,n)) 

sum(causa.cuerpo$n)

causa.cuerpo %>% 
  filter(causa_orig %in% "Indeterminado",
         !(lesion %in% "NA"),
         anio %in% c("2019","2020","2021")) %>% 
  ggplot(aes(causa_orig, reorder(lesion,n), fill = n)) +
  geom_tile(col = "black", alpha = .9) +
  labs(x = "Lesión",
       y = "Parte",
       title = "Lesión vs cuerpo",
       caption = "") +
  # scale_x_discrete(breaks=c("Ahogadas","Amputación","Anzuelos","Cortes","Enredadas","Fracturas",
  #                           "Herida","Nada","Parásitos","Petroleadas","Varias lesiones"),
  #                  label=c("Ahogadas","Amputación","Anzuelos","Cortes","Enredadas","Fracturas",
  #                          "Herida","Nada","Parásitos","Petroleadas","Varias")) +
  # scale_y_discrete(breaks=c("Varias","Ojos","Nada","Cuello",
  #                           "Caparazón","Cabeza","Boca","Aleta"),
  #                  label=c("Varias","Ojos","Nada","Cuello",
  #                          "Caparazón","Cabeza","Boca","Aleta")) +
  scale_fill_gradient(name="Nº tort.",
                      low = "skyblue", high = "red") +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        title = element_markdown(size = 11, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10),size = 13),
        axis.title.y = element_text(margin = margin(r = 10), size = 13),
        #axis.text.x = element_text(angle = 90,vjust = .16),
        plot.caption =  element_markdown(hjust = 0, face = "italic"),
        legend.title = element_text(hjust = -1.5),
        legend.key.size = unit(.45, "cm"),
        legend.text = element_text(size = 7))


################################################################################
#                         Tortugas de tierra                                   #
################################################################################


tor_tierra <- df_tortugas2 %>%
  filter(
    especie %in% c("Centrochelys sulcata",
                   "Chelonoidis carbonaria",
                   "Chelyda serpentina")
  ) %>% 
  select(observa) %>% view()

