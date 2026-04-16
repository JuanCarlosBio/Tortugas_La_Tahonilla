###############################################################################
#### Estudio de las tortugas según años / estaciones / meses de su llegada ####
###############################################################################

source("1Lib_DFs.R")

# Estudio de la llegada de las tortugas al C.R a lo largo de los años

tor_anio_mes <- expand.grid(anio=as.character(levels(df_tortugas_marinas$anio)),
                            mes=as.character(levels(df_tortugas_marinas$mes))) %>% left_join(
                              df_tortugas_marinas %>% group_by(anio,mes) %>% summarize(n=n()),
                              by=c("anio","mes")
                            ) %>% mutate(n=ifelse(is.na(n),0,n))

tor_anio_est <- expand.grid(anio=as.character(levels(df_tortugas_marinas$anio)),
                            estacion=as.character(levels(df_tortugas_marinas$estacion))) %>% left_join(
                              df_tortugas_marinas %>% group_by(anio,estacion) %>% summarize(n=n()),
                              by=c("anio","estacion")
                            ) %>% mutate(n=ifelse(is.na(n),0,n))



#view(tor_anio_mes)

# Número de tortugas que llegan al año

n_tortugas <- df_tortugas_marinas %>% 
  group_by(anio) %>% 
  summarise(n=n()) %>%
  select(n)
#view(n_tortugas)
x <- c(1:22)

y <- n_tortugas$n+20
etiquetas_ntortugas <- tibble(x=x,
                              y=y,
                              num_tor=n_tortugas$n)


gfe_bar_n <- tor_anio_mes %>% 
  ggplot(aes(anio, n)) +
  geom_bar(stat = "identity", position = position_stack(),
           alpha = .8, width = .5) +
  geom_text(data = etiquetas_ntortugas, 
            aes(x=x,y=y, label=num_tor), inherit.aes= FALSE, size=3.75) +
  scale_fill_manual(values = c("red4","red3","red","gray35","gray55",
                               "gray","blue4","blue",
                               "skyblue","yellow4","yellowgreen","yellow")) +
  labs(
    title = "Turtles arrival every year",
    x = "Year",
    y = "Num.turtles",
    fill = NULL) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,220)) +
  theme(
        #panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        #plot.background = element_rect(fill = "lightblue", color = "lightblue"),
        axis.line.x  = element_line(),
        title = element_markdown(size = 11, face = "bold"),
        plot.title = element_markdown(margin = margin(b = 1, unit = "lines"), hjust = .5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 270, hjust = 1, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 13),
        axis.text = element_text(size = 10.5),
        plot.caption =  element_markdown(hjust = 0, face = "italic"));gfe_bar_n


# Ahora a lo largo de los años, pero estudiando la estación en la que llegan al C.R.
# Me gusta más como grafico de líneas y dispersión, por la cantidad de años 

graf_est <- tor_anio_est %>% 
  ggplot(aes(anio, n, col = estacion, group = estacion)) +
  #geom_bar(stat = "identity", position = position_dodge(),
  #         col = "black", alpha = .8) +
  geom_point(size = 2) +
  geom_line(size = .75) +
  scale_color_manual(
                     breaks  = c("Primavera","Verano",
                                 "Otoño","Invierno"),
                     labels = c("Spring","Summer","Fall","Winter"), 
                     values = c("yellowgreen","darkmagenta","orangered",
                                "cyan3")) +
  labs(title = "Arrival according to season and year",
       x = "Year",
       y = "Num.turtles",
       col="Season") +
       # caption = "Fig 4. Se ven la llegada de las tortugas a la Tahonilla según la estación, parece
       # haber una tendencia en la incidencia sobre todo en la estación de verano, mientras que la estación
       # en la que llegan menos suele ser en invierno.") 
  theme_classic() +
  theme(title = element_markdown(size = 12, face = "bold"),
        panel.grid = element_blank(),
        #panel.background = element_rect(fill = "white", color = "azure"),
        #plot.background = element_rect(fill = "lightblue", color = "lightblue"),
        plot.title = element_markdown(margin = margin(b = 1, unit = "lines"),
                                      hjust = .5),
        axis.text = element_text(size = 10.5),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_markdown(angle = 270, vjust = .4),
        axis.title.x = element_text(margin = margin(t = 10),size = 13),
        axis.title.y = element_text(margin = margin(r = 10), size = 13),
        plot.caption =  element_markdown(hjust = 0, face = "italic"),
        legend.background =  element_rect(fill = "white"),
        #legend.key = element_rect(fill = "white"),
        legend.position = "top");graf_est


### Análisis estadístico
## Estdio de la normalidad
tapply(tor_anio_est$n, tor_anio_est$estacion, shapiro.test)         

#### Histograma para el estudio de la distribución de los datos####

gfe_hist <- tor_anio_est %>% 
  mutate(estacion=case_when(estacion == "Primavera" ~ "Spring",
                            estacion == "Verano" ~ "Summer",
                            estacion == "Otoño" ~ "Fall",
                            estacion == "Invierno" ~ "Winter"),
         estacion=factor(estacion, levels=c("Spring","Summer","Fall","Winter"))) %>% 
  ggplot(aes(n,fill=estacion)) +
  geom_histogram(bins = 25,col="black", show.legend = F) +
  facet_wrap(~ estacion, ncol=2) +
  labs(title = "Distribution of the data",
       x= "Num.turtles",
       y = "Frecuency",
       subtitle = "") +
  scale_fill_manual(breaks = c("Spring", "Summer", "Fall", "Winter"),
                    values = c("yellowgreen","darkmagenta","orangered",
                               "cyan3")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,10)) +
  theme_test() +
  theme(plot.title = element_text(size = 13,
                                  #margin = margin(b=1, unit = "lines"),
                                  face = "bold",
                                  hjust = .5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        #plot.background = element_rect(fill = "lightblue2",color = "lightblue2"),
        plot.subtitle  =element_text(size = 9),
        axis.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        strip.background = element_blank(),
        strip.text = element_markdown(face = "bold"));gfe_hist

### Kruskal-Wallis y Dunn-Test
library(FSA)
KWE <- kruskal.test(n~estacion, data = tor_anio_est);KWE
KWE$statistic
KWE$p.value
dunnTest(n~estacion, data = tor_anio_est,
         method="bonferroni")

etiquetas_2 <- tibble(x = c(1.35,2.35,3.35,4.35),
                      y = c(25,45,25,10),
                      label = c("b","a","b","c"))

gfe_kw <- tor_anio_est %>%
  ggplot(aes(estacion, n, fill = estacion)) +
  geom_jitter(pch = 21, position = position_jitterdodge(1.25, seed = 20101997),
              alpha = .8,show.legend = F) +
  geom_boxplot(alpha=.5,width=.5,show.legend = F) +
  geom_text(data = tibble(x=c(1.3,2.3,3.3,4.3), y=c(28,55,30,15)),
               aes(x=x,y=y, label=c("b","a","b","c")), inherit.aes = FALSE) +
  labs(title = "Arrival to the CRFS by the season",
       subtitle = "*X\u00B2 Kruskal-Wallis* = 56.56, *p* = 3.19e-12***<br>Post-hoc: Dunnet Test (correction: Bonf.)",
       y = "Num.turtles",
       x = "Season",
       fill=NULL) +
  theme_classic() +
  scale_fill_manual(breaks  = c("Primavera","Verano",
                                "Otoño","Invierno"),
                    values = c("yellowgreen","darkmagenta","orangered",
                               "cyan3")) +
  scale_x_discrete(breaks= c("Primavera","Verano",
                             "Otoño","Invierno"),
                   labels=c("Primavera\n(n=22)","Verano\n(n=22)",
                            "Otoño\n(n=22)","Invierno\n(n=22)")) +
  theme(plot.title = element_markdown(size = 11, face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        #plot.background = element_rect(fill = "lightblue2", color = "lightblue"),
        axis.line.y = element_line(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 13, face = "bold",margin = margin(t = 10)),
        axis.title.y = element_text(size = 13, face = "bold",margin = margin(r = 10)),
        axis.text.x = element_text(size = 12), 
        plot.subtitle = element_markdown(),
        legend.position = c(.85,.7),
        legend.background = element_rect(color = "black"));gfe_kw


## Tabka de estadísticos descriptivos

tabla_est_a.e <- tor_anio_est %>%
  group_by(estacion) %>% 
  summarise(num_tor=sum(n),
            minimo = min(n),
            q1 = quantile(n,.25),
            mediana = median(n),
            q3 = quantile(n,.75),
            maximo = max(n)) %>% view()
#write.xlsx(tabla_est_a.e,'tabla_est_a_e.xlsx')

########################################################################################
###################       Juantar los gráficos mediante cowplot         ################
########################################################################################


plot_grid(gfe_bar_n,graf_est,
          nrow = 2,
          rel_heights = c(.5,.75),
          labels = c("A","B"))

ggsave("temporal1.png", path = "C:\\Users\\jcge9\\Desktop\\TFG\\Tortugas_La_Tahonilla\\graficas",
       width = 7, height = 6)

#-------------------------------------------------------------------------------#


plot_grid(gfe_hist,gfe_kw,
          rel_widths = c(.4,.6),
          labels = c("A","B"))

ggsave("Rplot01.png", path = "C:\\Users\\jcge9\\Desktop\\TFG\\Tortugas_La_Tahonilla\\graficas",
       width = 6.5, height = 3.5)


