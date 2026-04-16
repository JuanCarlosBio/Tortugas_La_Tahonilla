##
## Este script en concreto es basura, pero lo guardo porque todo código puede ser reutilizable
##

################################################################################
############## Tabla de las causas según la evolución de los años  #############
################################################################################


c_a <- expand.grid(causa_orig=as.character(unique(as.character(df_tortugas_marinas$causa_orig))),
                          anio=as.character(unique(as.character(df_tortugas_marinas$anio)))) %>% left_join(
                            df_tortugas_marinas %>% group_by(causa_orig,anio) %>% summarize(n=(n())),
                            by=c("causa_orig","anio")
                          ) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  mutate(anio=factor(anio,
                     levels = c("2001","2002","2003","2004","2005","2006",
                                "2007","2008","2009","2010","2011","2012",
                                "2013","2014","2015","2016","2017","2018",
                                "2019","2020","2021","2000"))) %>% 
  filter(!(anio %in% "2000")) %>% 
  arrange(as.numeric(anio))
  

## Causas humanas:

# "Anzuelo","Artes de pesca","Capturada",
# "Choque con embarcación","Choques","Decomisos",
# "Enmallada","Hidrocarburos","Rafia"


{
anzuelo <- filter(c_a, causa_orig  %in% "Anzuelo");anzuelo
artes_de_pesca <- filter(c_a, causa_orig %in% "Artes de pesca");artes_de_pesca
capturada <- filter(c_a, causa_orig %in% "Capturada");capturada
choque_con_embarcacion <- filter(c_a, causa_orig %in% "Choque con embarcación");choque_con_embarcacion
choques <- filter(c_a, causa_orig %in% "Choques");choques
decomisos <- filter(c_a, causa_orig %in% "Decomisos");decomisos
enmallada <- filter(c_a, causa_orig %in% "Enmallada");enmallada
hidrocarburos <- filter(c_a, causa_orig %in% "Hidrocarburos");hidrocarburos
rafia <- filter(c_a, causa_orig %in% "Rafia");rafia
  }


# Tabla de causas humanas
tabla_humanas <- data.frame(anio=c("2001","2002","2003","2004","2005","2006",
                  "2007","2008","2009","2010","2011","2012",
                  "2013","2014","2015","2016","2017","2018",
                  "2019","2020","2021"),
           anzuelo=anzuelo$n, artes_pesca=artes_de_pesca$n,
           capturada=capturada$n,choque_embarcación=choque_con_embarcacion$n,choque=choques$n,
           decomisos=decomisos$n,enmallada=enmallada$n,hidrocarburos=hidrocarburos$n,
           rafia=rafia$n) %>% view()

# fun_cero <- function(vector){
#   op1=vector==0
#   op2=sum(op1=="FALSE")
#   return(op2)
# }


# Gráfico exporatorio de las causas humanas: parece ser que las causas de artes
# de pesca disminyen respecto a los años, además los hidrocarburos parecen haber 
# descendido de valores "relativamente bajos" a muy bajos ~0. Las demás causas
# humanas parecen mantenerse constantes a lo largo de los años, con valores bajos.
# Las causas provocadas por anzuelos, capturadas, decomisos y rafia son inexistentes
# de los años 2000-2021.
c_a %>% filter(causa_orig %in% c("Artes de pesca","Choque con embarcación",
                                                "Choques","Enmallada","Hidrocarburos")) %>%
  filter(!(anio %in% "2000")) %>% 
  ggplot(aes(anio,n,col=causa_orig,group=causa_orig)) +
  geom_point(size=2) +
  geom_line(size=.75) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 270,vjust  = 0),
        legend.position = "bottom")

## Causas naturales

# "Debilidad agotamiento","Enfermedad",
# "Inanición","Muerte Natural","Soleándose"

{debilidad_agotamiento <- filter(c_a, causa_orig  %in% "Debilidad agotamiento");debilidad_agotamiento
enfermedad <- filter(c_a, causa_orig  %in% "Enfermedad");enfermedad
inanicion <- filter(c_a, causa_orig  %in% "Inanición");inanicion
muerte_natural <- filter(c_a, causa_orig  %in% "Muerte Natural");muerte_natural
soleandose <- filter(c_a, causa_orig  %in% "Soleándose");soleandose}

# Tabla de causas naturales
tabla_naturales <- data.frame(anio=c("2001","2002","2003","2004","2005","2006",
                  "2007","2008","2009","2010","2011","2012",
                  "2013","2014","2015","2016","2017","2018",
                  "2019","2020","2021"),
           debilidad_agotamiento=debilidad_agotamiento$n,enfermedad=enfermedad$n,
           inanicion=inanicion$n,muerte_antural=muerte_natural$n,
           soleandose=soleandose$n) %>% view()

# Gráfico exploratorio de las causas naturales de las tortugas llegadas a la tahonilla. 
# Lo que se puede ver es que la causa natural más habitual el son las enfermedades, sin
# embargo estas en la década de 2000 - 2010 es muy intermitente, aunque parece estabili_
# zarse un poco en la década de 2010-20, incluido el 2021, y sin embargo, los casos de 
# debilidad y agotamieto de las tortugas sin escasos, pero hay en la primera década y
# aumentan drásticamente en la segunda. Por otro ladocausas como Muerte Natural, inanición
# y soleándse no presentan valores siginc¡ficativos hasta los años 2018-2021

c_a %>% 
   filter(causa_orig %in% c("Debilidad agotamiento","Enfermedad",
                             "Soleándose","Inanición","Muerte Natural")) %>%
  filter(!(anio %in% "2000")) %>% 
  ggplot(aes(anio,n,col=causa_orig,group=causa_orig)) +
  geom_point(size=2) +
  geom_line(size=.75) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 270,vjust  = 0),
        legend.position = "bottom")

## Causas Indeterminadas

# Indeterminado, Otros, NA

{
  indeterminado <- filter(c_a, causa_orig %in% "Indeterminado")
  otros <- filter(c_a, causa_orig %in% "NA")
  na <- filter(c_a, causa_orig %in% "Otros")
  }

# Tabla de valores indeterminados-otros
tabla_indetr <- data.frame(
     anio=c("2001","2002","2003","2004","2005","2006",
            "2007","2008","2009","2010","2011","2012",
            "2013","2014","2015","2016","2017","2018",
            "2019","2020","2021"),
    indeterminadas=indeterminado$n,
    otros=otros$n,na=na$n) %>% view()

c_a %>% 
  filter(causa_orig %in% c("Indeterminado", "Otros", "NA")) %>%
  filter(!(anio %in% "2000")) %>% 
  ggplot(aes(anio,n,col=causa_orig,group=causa_orig)) +
  geom_point(size=2) +
  geom_line(size=.75) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 270,vjust  = 0),
        legend.position = "bottom")

#------------------------------------------------------------------------------#
#                   Tabla que mestra todas las causas juntas                   #
#------------------------------------------------------------------------------#

tabla_semicompleta <- inner_join(tabla_humanas, 
           tabla_naturales,
           by='anio') 

tabla_completa <- inner_join(tabla_semicompleta,tabla_indetr,
                             by='anio') %>% 
  mutate(anio = as.numeric(anio)) %>% view()

write.xlsx(tabla_completa,'tabla_causas_evo.xlsx')

{
cor_anz <- cor.test(tabla_completa$anio, tabla_completa$anzuelo)
cor_art.pes <- cor.test(tabla_completa$anio, tabla_completa$artes_pesca) 
cor_captu <- cor.test(tabla_completa$anio, tabla_completa$capturada)
cor_choq.barco <- cor.test(tabla_completa$anio, tabla_completa$choque_embarcación)
cor_choq <- cor.test(tabla_completa$anio, tabla_completa$choque)
cor_decom <- cor.test(tabla_completa$anio, tabla_completa$decomisos)
cor_enmall <- cor.test(tabla_completa$anio, tabla_completa$enmallada)
cor_hidro <- cor.test(tabla_completa$anio, tabla_completa$hidrocarburos)
cor_raf <- cor.test(tabla_completa$anio, tabla_completa$rafia)
cor_deb.ago <- cor.test(tabla_completa$anio, tabla_completa$debilidad_agotamiento)
cor_enf <- cor.test(tabla_completa$anio, tabla_completa$enfermedad)
cor_inan <- cor.test(tabla_completa$anio, tabla_completa$inanicion)
cor_muer.nat <- cor.test(tabla_completa$anio, tabla_completa$muerte_antural)
cor_sole<- cor.test(tabla_completa$anio, tabla_completa$soleandose)
cor_ind<- cor.test(tabla_completa$anio, tabla_completa$indeterminadas)
cor_otros<- cor.test(tabla_completa$anio, tabla_completa$otros)
cor_na<- cor.test(tabla_completa$anio, tabla_completa$na)
 }
tabla_correlacion <- data.frame(causa=c("Anzuelo","Artes de pesca","Capturada","Choque con embarcación",
                                        "Choques","Decomisos","Enmallamiento","Hidrocarburos","Rafia",
                                        "Debilidad y agotamiento","Enfermedad","Inanición","Muerte natural",
                                        "Soleándose","Indeterminadas","Otros","NA"),
                                cor_pearson=c(cor_anz$estimate,cor_art.pes$estimate,cor_captu$estimate,
                                              cor_choq.barco$estimate,cor_choq$estimate,cor_decom$estimate,
                                              cor_enmall$estimate,cor_hidro$estimate,cor_raf$estimate,
                                              cor_deb.ago$estimate,cor_enf$estimate,cor_inan$estimate,
                                              cor_muer.nat$estimate,cor_sole$estimate,cor_ind$estimate,
                                              cor_otros$estimate,cor_na$estimate)) %>% view(
                                              )
tabla_correlacion %>% 
  ggplot(aes(reorder(causa,cor_pearson),cor_pearson)) +
  geom_bar(stat = "identity",col="black",
           fill="gray95") +
  geom_hline(yintercept = 0,size=.5) +
  labs(title = "Correlación entre los años y las causas",
       x="Causa",
       y="Estimación (Pearson)") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(-1,1),
                     breaks = seq(-1,1,.25)) +
  geom_hline(yintercept=0.7039996, col="red",
             linetype="dashed") +
  geom_hline(yintercept=-0.634248, col="red",
             linetype="dashed") +
  geom_text(data = tibble(x=c(14.5,2.5),y=c(0.7039996+.1,-0.634248-.1)),
            aes(x=x,y=y,label=c("Máximo=0.7","Mínimo=-0.63")), inherit.aes = F,
            col="darkred") +
  theme(
    panel.background = element_blank(),
    axis.line.y = element_line(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size=13,face = "bold",
                              margin = margin(b=10)),
    axis.title = element_text(size = 12,face = "bold")
  )

round(cor_art.pes$estimate,2)
round(cor_choq.barco$estimate,2)
#------------------------------------------------------------------------------# 
#                           Porcentajes de las causas                          #
#------------------------------------------------------------------------------#

#### Todas las causas por separado

causa_anio %>% 
  filter(!(anio %in% "2000")) %>% 
  group_by(anio) %>% 
  mutate(porcentaje=(n/sum(n))*100, 
         anio=factor(anio,
                     levels = c(2001,2002,2003,2004,2005,2006,
                                2007,2008,2009,2010,2011,2012,
                                2013,2014,2015,2016,2017,2018,
                                2019,2020,2021)),
         causa_orig=factor(causa_orig,
                           levels = c("Anzuelo","Artes de pesca","Capturada",
                                      "Choque con embarcación","Choques","Decomisos",
                                      "Enmallada","Hidrocarburos","Rafia",
                                      "Debilidad agotamiento","Enfermedad",
                                      "Inanición","Muerte Natural","Soleándose",
                                      "Indeterminado", "Otros", "NA"))) %>% 
  ggplot(aes(anio,porcentaje,group=causa_orig,fill=causa_orig)) +
  geom_bar(stat = "identity",col="black")+
  coord_flip() +
  scale_fill_manual(values = c("black","white","darkgray","yellow",
                               "orange","green","aquamarine2","red",
                               "bisque","yellowgreen","blue","turquoise",
                               "wheat","seashell3","limegreen","cornflowerblue","magenta2")) +
  theme(
    legend.key.height = unit(.4,"cm") 
  )


#### Las causas recogidas en causas humanas, naturales, indeterminadas-otras      

dfc_completo %>% 
  filter(!(anio %in% "2000")) %>% 
  group_by(anio) %>% 
  mutate(porcentaje=(n/sum(n))*100, 
         anio=factor(anio,
                     levels = c(2001,2002,2003,2004,2005,2006,
                                2007,2008,2009,2010,2011,2012,
                                2013,2014,2015,2016,2017,2018,
                                2019,2020,2021))) %>% 
  ggplot(aes(anio,porcentaje,group=causa,fill=causa)) +
  geom_bar(stat = "identity",col="black")+
  coord_flip() +
  scale_fill_manual(values = c("tomato","yellowgreen","skyblue")) +
  theme(
    legend.key.height = unit(.4,"cm") 
  )

citation()

