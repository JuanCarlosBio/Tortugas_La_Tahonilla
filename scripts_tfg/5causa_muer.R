##########################
#### Causas y muertes ####
##########################

source("1Lib_DFs.R")

# Causas del 2000
expand.grid(anio=as.character(unique(as.character(df_tortugas_marinas$anio))),
            causa=as.character(unique(as.character(df_tortugas_marinas$causa)))) %>% left_join(
                              df_tortugas_marinas %>% group_by(anio,causa) %>% summarize(n=(n())),
                              by=c("anio","causa")
                            ) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  filter(anio %in% "2000") %>%
  group_by(causa) %>% 
  summarise(n = sum(n))

Humanos_2000 <- 1+1+4
Naturales_2000 <- 2
No_clasificado <- 33+26+7

Total <- Humanos_2000 + Naturales_2000 + No_clasificado

round(No_clasificado/Total,4)*100

df_tm_causas <- df_tortugas_marinas %>% filter(anio != "2000")
causa_muerte <- expand.grid(causa=as.character(unique(as.character(df_tm_causas$causa))),
                            muerte=as.character(unique(as.character(df_tm_causas$muerte)))) %>% left_join(
                              df_tm_causas %>% group_by(causa,muerte) %>% summarize(n=(n())),
                              by=c("causa","muerte")
                            ) %>% mutate(n=ifelse(is.na(n),0,n))

sum(causa_muerte$n)
cm_no_indeterninado <- causa_muerte %>% filter(causa %in% c("Enfermedad","Artes de pesca","Otros",
                                                            "Debilidad agotamiento","Hidrocarburos",         
                                                            "Choques","Choque con embarcación",
                                                            "Enmallada","Soleándose"));cm_no_indeterninado
cm_no_indeterninado %>% summarise(x=sum(n))

cm_indeterminado <- filter(causa_muerte, causa %in% c("Indeterminado", "NA")) %>% 
  group_by(muerte) %>% summarise(n=sum(n)) %>% mutate(causa=c("Indeterminado","Indeterminado"),
                                                      muerte=c("No","Si"),
                                                      n=n);cm_indeterminado
cm_indeterminado %>% summarise(x=sum(n))


c = c(cm_no_indeterninado$causa, cm_indeterminado$causa);c
m = c(cm_no_indeterninado$muerte, cm_indeterminado$muerte);m
n = c(cm_no_indeterninado$n,cm_indeterminado$n);n


cm <- data.frame(causa=c,
                 muerte=m,
                 n=n);view(cm)    


# Tabla de datos de las causas, desde el año 2000-2021

causa_No <- cm %>% filter(muerte %in% "No") %>% 
  mutate(Supervivientes=n) %>% select(causa,Supervivientes);causa_No
causa_Si <- filter(cm, muerte %in% "Si") %>% 
  mutate(Fallecidas=n) %>% select(causa,Fallecidas);causa_Si

causa_no_si <- left_join(causa_No,causa_Si, by='causa');causa_no_si


tabla_causas <- cm %>% 
  group_by(causa) %>% 
  summarise(n=sum(n)) %>% 
  mutate(frecuencia=round(n/sum(n),4)) %>%   
  arrange(desc(n));tabla_causas

tabla_causas_completa <- left_join(tabla_causas, causa_no_si,
                                   by='causa');tabla_causas_completa

#write.xlsx(as.data.frame(tabla_causas), file = "C:/Users/jcge9/Desktop/TFG/TFG_R/tabla_causas.xlsx")






######################################   Juntar   #########################################
# 
# Causas humanas: Artes de pesca, choques, hidrocarburos, enmallada, choque con embarcación.
# Causas naturales:Enfermedad, Debilidad y agotamiento, soleándose.
# Indeterminados-otros: Indeterminado, NA, Otros.

causa_anio <- expand.grid(causa_orig=as.character(unique(as.character(df_tortugas_marinas$causa_orig))),
                          anio=as.character(unique(as.character(df_tortugas_marinas$anio)))) %>% left_join(
                          df_tortugas_marinas %>% group_by(causa_orig,anio) %>% summarize(n=(n())),
                          by=c("causa_orig","anio")
                              ) %>% mutate(n=ifelse(is.na(n),0,n))  %>% 
  filter(!(anio %in% "2000")) %>% view()

causa_anio %>% group_by(causa_orig) %>% summarise(n=n())
sum(causa_anio$n)
  
df_causas_h <- causa_anio %>% filter(causa_orig %in% c("Anzuelo","Artes de pesca","Capturada",
                                                  "Choque con embarcación","Choques","Decomisos",
                                                  "Enmallada","Hidrocarburos","Rafia")) %>% 
  group_by(anio) %>% 
  summarise(n=sum(n)) %>% 
  mutate(causa=rep("Causas humanas",21))

sum(df_causas_h$n)

  
df_causas_n <- causa_anio %>% filter(causa_orig %in% c("Debilidad agotamiento","Enfermedad",
                                                  "Inanición","Muerte Natural","Soleándose")) %>% 
  group_by(anio) %>% 
  summarise(n=sum(n)) %>% 
  mutate(causa=rep("Causas naturales",21))
  
sum(df_causas_n$n)

df_causas_i <- causa_anio %>% filter(causa_orig %in% c("Indeterminado", "Otros", "NA")) %>% 
 group_by(anio) %>% 
  summarise(n=sum(n)) %>% 
  mutate(causa=rep("Indet./Otros",21))

sum(df_causas_i$n)

dfc_completo <- data.frame(anio=c(df_causas_h$anio,df_causas_i$anio,df_causas_n$anio),
                           causa=c(df_causas_h$causa,df_causas_i$causa,df_causas_n$causa),
                           n=c(df_causas_h$n,df_causas_i$n,df_causas_n$n)) %>% 
  mutate(anio=factor(anio,
                     levels = c("2001","2002","2003","2004",
                                "2005","2006","2007","2008","2009",
                                "2010","2011",
                                "2012","2013","2014","2015","2016",
                                "2017","2018","2019","2020","2021")));view(dfc_completo)

#-------------------------------------------#
# Tabla de estaísticos descriptivos         #
# Causas humanas, naturales, indeterminadas #
#-------------------------------------------#

# 2001-2012
tabla.est.0112 <- dfc_completo %>% 
  filter(anio %in% c("2001","2002","2003","2004","2005","2006",
                     "2007","2008","2009","2010","2011","2012")) %>%   
  group_by(causa) %>% 
  summarise(num_tor=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n));tabla.est.0112
# 2013-2021
tabla.est.1321 <- dfc_completo %>% 
  filter(!(anio %in% c("2001","2002","2003","2004","2005","2006",
                     "2007","2008","2009","2010","2011","2012"))) %>%   
  group_by(causa) %>% 
  summarise(num_tor=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n));tabla.est.1321

# write.xlsx(tabla.est.0112,'tabla.est.0112.xlsx')
# write.xlsx(tabla.est.1321,'tabla.est.1321.xlsx')


#-----------------------------------------------------------#
# Gráficos de las causas humanas, indeterminados, naturales #
#-----------------------------------------------------------#

dfc_completo %>% 
  filter(anio != "2000") %>% 
  #group_by(causa) %>% 
  ggplot(aes(anio,n,col = causa,group=causa)) +
  geom_point(size = 2) +
  geom_line(size = .75) +
  #geom_smooth(method = "lm", se =F, size=.75) +
  labs(title = "Evolution of the causes",
       x = "Year",
       y = "Num.turtles",
       col = "Causes") +
  scale_color_manual(
                     breaks = c("Causas humanas","Causas naturales",
                                "Indet./Otros"),
                     labels= c("Anthropogenics","Naturals","Unclassified"),
                     values = c("pink","yellowgreen","skyblue2"),
                     ) + 
  theme_classic() +
  theme(
        # plot.background = element_rect(fill = "lightblue1", color = "lightblue1"), 
        panel.background = element_blank(), 
        plot.title = element_markdown(hjust = .5),
        title = element_markdown(size = 11, face = "bold"),
        #panel.background = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 13),
        axis.title.y = element_text(margin = margin(r = 10), size = 13),
        axis.text.x = element_text(angle = 270, vjust  = 0.5),
        axis.text = element_text(size = 10.5),
        plot.caption = element_markdown(hjust = 0, face = "italic"),
        legend.position = "top",
        legend.background = element_rect(fill = "white"))

 ggsave("causas_evolucion.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\Tortugas_La_Tahonilla\\graficas",
       width = 7,
       height = 4)


# Comparación de los grupos, Causas humanas, causas naturales, Indeterminados-otros
# de la última década.

dfc_ult.decada <- dfc_completo %>% 
  filter(anio %in% c("2013","2014","2015","2016",
                     "2017","2018","2019","2020","2021"))

tapply(dfc_ult.decada$n,dfc_ult.decada$causa,shapiro.test)                      # p>0.05
c_kw1 <- kruskal.test(n~causa, data = dfc_ult.decada)
ckw1_estadistico <- round(c_kw1$statistic,2)
ckw1_pvalue <- round(c_kw1$p.value,2)

gfc_hist.1 <- dfc_ult.decada %>%
  mutate(causa=factor(causa,
                      levels = c("Causas humanas","Causas naturales",
                                 "Indet./Otros"),
                      labels = c("Anthropogenics","Naturals","Non clasificated"))) %>% 
  ggplot(aes(n, fill = causa)) +
  geom_histogram(col = "black", bins = 20,
                 show.legend = F) +
  # geom_text(data=hist_text.c1,
  #           aes(x=x,y=y, label=lab,col=causas),
  #           inherit.aes = F) +
  facet_wrap(~causa, ncol = 2) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  labs(title = "Distribution of the data",
       subtitle = "Years: 2013-21",
       x="Num.turtles",
       y="Frecuency") +
  theme_test() +
  theme(
    #plot.background = element_rect(fill = "lightblue2", color = "lightblue2"), 
    panel.background = element_blank(),
    plot.title = element_text(size = 12,
                              #margin = margin(b=1, unit = "lines"),
                              face = "bold",
                              hjust = .5),
    plot.subtitle  =element_text(size = 10, hjust = .5),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t=10)),
    axis.title.y = element_text(margin = margin(r=10)),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"));gfc_hist.1

#### K-W 1

gfc_anova <- dfc_ult.decada %>% 
  mutate(causa=factor(causa,
                      levels = c("Causas humanas","Causas naturales",
                                 "Indet./Otros"),
                      labels = c("Anthropogenics","Naturals","Non clasificated"))) %>%
  ggplot(aes(causa,n,fill=causa)) +
  geom_jitter(pch=21, 
              position = position_jitterdodge(.5,seed = 20101997),
              show.legend = F) +
  geom_boxplot(alpha=.5, width=.5,
               show.legend = F) +
  # stat_summary(fun = mean, geom = "crossbar", 
  #              width = .5, show.legend = F) +
  labs(title = "Comparison between the 2013-21 causes",
       subtitle = glue("*X\u00B2 Kruskal-Wallis* = {ckw1_estadistico}; *p* = {ckw1_pvalue}"),
        x = "Causes",
        y = "Num.turtles") +
  scale_x_discrete(breaks=c("Anthropogenics","Naturals","Non clasificated"),
                   labels=c("Anthropogenics\n(n=9)", "Naturals\n(n=9)","Unclassified\n(n=9)")) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  theme_classic() +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"), 
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 12,
                              face = "bold",
                              hjust = .5,
                              margin = margin(b=1,unit = "lines")),
    plot.subtitle = element_markdown(size = 10.5, hjust = .5),
    #panel.background = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10), size = 12,face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12,face = "bold"),
    axis.text.x = element_text(size = 11), 
    plot.caption = element_markdown(hjust = 0, face = "italic"),
    legend.position = c(.5,.85),
    legend.background = element_rect(color = "black"),
    legend.key.size = unit(.4,"cm"));gfc_anova

# Comparación de los grupos 

dfc_2000_2011 <- dfc_completo %>% 
  filter(anio %in% c("2001","2002","2003","2004",
                     "2005","2006","2007","2008","2009",
                     "2010","2011","2012")) 

tapply(dfc_2000_2011$n,dfc_2000_2011$causa,shapiro.test)                        
c_kw2 <- kruskal.test(n~causa, data = dfc_2000_2011)                                     
dunnTest(n~as.factor(causa), data = dfc_2000_2011,
            method="bonferroni")
c_kw2_estadistico <- round(c_kw2$statistic,2)
c_kw2_pvalue <- round(c_kw2$p.value,7)
##### Histograma

gfc_hist2 <- dfc_2000_2011 %>% 
  mutate(causa=factor(causa,
                      levels = c("Causas humanas","Causas naturales",
                                 "Indet./Otros"),
                      labels = c("Anthropogenics","Naturals","Unclassified"))) %>% 
  ggplot(aes(n, fill = causa)) +
  geom_histogram(col = "black", bins = 20,
                 show.legend = F) +
  facet_wrap(~causa, ncol = 2) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  labs(title = "Distribution pf the data",
       subtitle = "Years: 2001-12",
       x="Num.turtles",
       y="Frecuency") +
  theme_test() +
  theme(
    #plot.background = element_rect(fill = "lightblue2", color = "lightblue2"), 
    panel.background = element_blank(),
    plot.title = element_text(size = 12,
                              #margin = margin(b=1, unit = "lines"),
                              face = "bold",
                              hjust = .5),
    plot.subtitle  =element_text(size = 10, hjust = .5),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t=10)),
    axis.title.y = element_text(margin = margin(r=10)),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"));gfc_hist2

##### K-W test

gfc_kw <- dfc_2000_2011 %>% 
  ggplot(aes(causa,n,fill=causa)) +
  geom_jitter(pch=21, 
              position = position_jitterdodge(.35,seed = 20101997),
              show.legend = F) +
  geom_boxplot(alpha=.5, width=.5,
               show.legend = F) +
  # stat_summary(fun = median, geom = "crossbar", 
  #              width = .35, show.legend = F) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  scale_x_discrete(breaks=c("Causas humanas", "Causas naturales","Indet./Otros"),
                   labels=c("Anthropogenics","Naturals","Unclassified")) +
  labs(title = "Comparison of the 2001-12 causes",
       subtitle = glue("*X\u00B2 Kruskal-Wallis* = {c_kw2_estadistico}; *p* = {c_kw2_pvalue}***<br>Post-hoc: Dunnet Test (correction: Bonf.)"),
       x = "Causes",
       y = "Num.turtles") +
  theme_classic() +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"), 
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    plot.title = element_markdown(size = 11.5, face = "bold", hjust = .5),
    plot.subtitle  =element_markdown(size = 10.5, hjust = .5),
    #panel.background = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold",margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold",margin = margin(r = 10)),
    axis.text.x = element_text(size = 11), 
    plot.caption = element_markdown(hjust = 0, face = "italic"),
    legend.position = c(.75,.7),
    legend.background = element_rect(color = "black"),
    legend.key.size = unit(.4,"cm"));gfc_kw

#########################################################################################
############################ Juntar los gráficos a analizar #############################
#########################################################################################


### K-W 1

plot_causas1 <- plot_grid(gfc_hist.1,gfc_anova,
          rel_widths = c(.4,.6),
          labels = c("A","B"))
plot_causas1

ggsave("causa3.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\Tortugas_La_Tahonilla\\graficas",
        width=7, height=3.5)

### K-W 1

plot_causas2 <- plot_grid(gfc_hist2,gfc_kw,
          rel_widths = c(.4,.6),
          labels = c("A","B"))
plot_causas2

 ggsave("causas2.png",
        path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\Tortugas_La_Tahonilla\\graficas",
        width=7, height=3.5)


plot_grid(plot_causas2,
          plot_causas1,
          nrow = 2)


################################################################################
############### Muertes de las tortugas a lo largo de los años  ################
################################################################################

anio_muerte <- expand.grid(anio=as.character(unique(as.character(df_tortugas_marinas$anio))),
                            muerte=as.character(unique(as.character(df_tortugas_marinas$muerte)))) %>% left_join(
                              df_tortugas_marinas %>% group_by(anio,muerte) %>% summarize(n=(n())),
                              by=c("anio","muerte")
                            ) %>% mutate(n=ifelse(is.na(n),0,n)) %>% arrange(anio)
anio_muerte %>% 
  group_by(muerte) %>% 
  summarise(num_tor=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n))
 


tapply(anio_muerte$n, anio_muerte$muerte, shapiro.test)
wilcox1 <- wilcox.test(n~factor(muerte), data = anio_muerte)
wilcox1_estadistico <- round(wilcox1$statistic,2)
wilcox1_pvalue <- round(wilcox1$p.value, 9)

# Histograma para ver la distribución de los datos
muerte_hist <- anio_muerte %>% 
  mutate(muerte=factor(muerte,
                       levels = c("No", "Si"),
                       labels = c("Alive", "Dead"))) %>% 
  ggplot(aes(n, fill=muerte)) +
  geom_histogram(col="black",alpha=.75, 
                 show.legend = F) +
  labs(title = "Distribution of the data",
       x="Num.turtles",
       y="Frecuency")+
  facet_wrap(.~muerte, nrow = 2) +
  scale_fill_manual(breaks=c("Alive","Dead"),
                    values = c("white","black")) +
  theme_test() +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"), 
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    #axis.text = element_text(size=12)
    # axis.title.x = element_text(margin = margin(r=10)),
    # axis.title.x = element_text(margin = margin(r=10))
  );muerte_hist
  

# Gráfico de cajas entre las tortugas que sobreviven y no 
muerte_cajas <- anio_muerte %>% 
  mutate(muerte=factor(muerte,
                       levels = c("No", "Si"),
                       labels = c("Alive", "Dead"))) %>%
  ggplot(aes(muerte,n,fill=muerte)) +
  geom_jitter(pch=21, show.legend = F, 
              position = position_jitterdodge(1,seed = 20101997),
              ) +
  geom_boxplot(alpha=.5, width=.5, show.legend = F) +
  labs(title = "<span style = 'color: gray'>Alive</span>/Dead on the arrival",
       subtitle = glue("*W* = {wilcox1_estadistico}; *p* = {wilcox1_pvalue}***"),
       x="Condition",
       y="Num.turtles") +
  scale_fill_manual(values = c("white","black")) +
  scale_x_discrete(breaks=c("Alive","Dead"),
                   labels=c("Alive\n(n=22)","Dead\n(n=22)")) +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_markdown(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_markdown(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    axis.text.x = element_text(size=12),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  );muerte_cajas

# Ambos gráficos juntos

plot_grid(muerte_hist, muerte_cajas,
          cols = 2,rel_widths = c(.4,.6),
          labels = c("A","B"))

no <- anio_muerte %>% filter(muerte %in% "No")
si <- anio_muerte %>% filter(!(muerte %in% "No"))

#no$anio == si$anio

tabla_muerte <- data.frame(anio = no$anio,
                           Sobrevive = no$n,
                           Fallece = si$n) %>% 
  arrange(anio);tabla_muerte
sum(tabla_muerte$Sobrevive) + sum(tabla_muerte$Fallece)

#write.xlsx(tabla_muerte, 'muerte.xlsx')
sum(anio_muerte$n)

################################################################################
############################  Tortugas rehabilitadas ###########################
################################################################################

df_rehabilitadas <-  read_excel("data_tortugas_tfg.xlsx", 
                                sheet = "Hoja2") %>% 
  filter(Especie %in% c("Caretta caretta","Chelonia mydas",
                        "Dermochelys coriacea","Eretmochelys imbricata",
                        "Lepidochelys olivacea"))

tor_no_rehab <- df_tortugas_marinas %>% 
  filter(ficha %in% df_rehabilitadas$`Nº Ficha`) %>% 
  mutate(rehabilitadas = rep("No rehab.",352)) %>%
  select(rehabilitadas,anio) 

tor_rehab <- df_tortugas_marinas %>% 
  filter(!(ficha %in% df_rehabilitadas$`Nº Ficha`),
         muerte %in% "No") %>% 
  mutate(rehabilitadas = rep("Rehab.",1334)) %>% 
  select(rehabilitadas,anio)


r_nor <- data.frame(rehabilitadas=c(tor_no_rehab$rehabilitadas,
                                            tor_rehab$rehabilitadas),
                            anio=c(tor_no_rehab$anio,
                                   tor_rehab$anio)) 

rehab_plis <- expand.grid(rehabilitadas=as.character(unique(as.character(r_nor$rehabilitadas))),
                          anio=as.character(unique(as.character(r_nor$anio)))) %>% left_join(
                             r_nor %>% group_by(anio,rehabilitadas) %>% summarize(n=(n())),
                             by=c("anio","rehabilitadas")
                             ) %>% mutate(n=ifelse(is.na(n),0,n)) %>% arrange(anio)

tapply(rehab_plis$n, rehab_plis$rehabilitadas, shapiro.test)
wilcox1_m <- wilcox.test(n~as.factor(rehabilitadas), data = rehab_plis)
wilcox1_m_estadistico <- round(wilcox1_m$statistic,2)
wilcox1_m_pvalue <- round(wilcox1_m$p.value,9)

hist_rehab <- rehab_plis %>% 
  ggplot(aes(n, fill=rehabilitadas)) +
  geom_histogram(col="black",alpha=.75, 
                 show.legend = F) +
  labs(title = "Distripubtion of the data",
       x="Num.turtles",
       y="Frecuency")+
  # # scale_y_continuous(expand = expansion(0),
  # #                    limits = c(0,13)) +
   scale_fill_manual(breaks=c("No rehab.","Rehab."),
                     values = c("darkgray","orange")) +
  facet_wrap(.~rehabilitadas, nrow = 2) +
  theme_test() +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"), 
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    text=element_text(family="Times New Roman"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
    #axis.text = element_text(size=12)
    # axis.title.x = element_text(margin = margin(r=10)),
    # axis.title.x = element_text(margin = margin(r=10))
  );hist_rehab


# Gráfico de cajas entre las tortugas que sobreviven y no 
boxplot_rehab <-  rehab_plis %>% 
  mutate(rehabilitadas = factor(
    rehabilitadas,
    levels = c("Rehab.", "No rehab.")
  )) %>% 
  ggplot(aes(rehabilitadas,n,fill=rehabilitadas)) +
  geom_jitter(pch=21, show.legend = F, 
              position = position_jitterdodge(1,seed = 20101997)) +
  geom_boxplot(alpha=.5, width=.5, show.legend = F) +
  labs(title = "<span style = 'color: gray45'>Rehabilitates</span>/<span style = 'color: orange'>No rehabilitates</span>",
       subtitle = glue("*W* = {wilcox1_m_estadistico}; *p* = {wilcox1_m_pvalue}***"),
       x="Condition",
       y="Num.turtles") +
  scale_x_discrete(breaks=c("Rehab.","No rehab."),
                   label=c("Rehab.\n(n=22)","No rehab.\n(n=22)")) +
  scale_fill_manual(values = c("gray45","orange")) +
  theme_classic() +
  theme(
    # plot.background = element_rect(fill = "lightblue2", color = "lightblue2"), 
    panel.grid = element_blank(),
    plot.title = element_markdown(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_markdown(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    axis.text.x = element_text(size=12)
    # axis.title.x = element_text(margin = margin(r=10)),
    # axis.title.x = element_text(margin = margin(r=10))
  );boxplot_rehab

plot_grid(muerte_hist, muerte_cajas,
          hist_rehab,boxplot_rehab,
          cols = 2,rel_widths = c(.4,.6),
          labels = c("A","B",
                     "C","D"))

ggsave("muerte.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\Tortugas_La_Tahonilla\\graficas",
       width=7,
       height=5)


#-------------------------------------------------------------------------#
# Tabla de datos de estadísticos descriptivos... a ver como lo hacemos... #
#-------------------------------------------------------------------------#


rehab_plis %>% 
  group_by(rehabilitadas) %>% 
  summarise(sum=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n))




