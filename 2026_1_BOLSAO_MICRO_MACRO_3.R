library(AgroR)
library(ggplot2)
library(readxl)
library(multcomp)
library(emmeans)
library(dplyr)
library(ggpubr)


setwd("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/2° Teste")


df = read_excel("RESULTADOS.xlsx",
                sheet = "CONDENSAÇÃO",
                range = "A1:D433"
                )

df$CONDENSACAO <- ifelse( df$CONDENSACAO == "SIM", 1, 0)

trat = df$BOLSAO
resp = df$CONDENSACAO #1 ou 0
a = glm(resp ~ trat, family = "binomial")


letra = 
  cld(
    regrid(
      emmeans(
        a, "trat", level = 0.95) #level = 1 - alpha.t
    ),
    Letters = letters, 
    reversed = TRUE, 
    adjusted = "tukey"
  )


letra %>% 
  select(-c(3,4)) %>% 
  mutate(
    across( c(2, 3, 4), ~ round(.*100, 2) )
  ) %>%  
  rename(
    inferior = 3,  
    superior = 4,  
    grupo = 5
  ) %>% 
  mutate(
    letra = paste( prob, "%", grupo , sep = "" )
  ) %>% 
  ggplot( aes(x = trat, y = prob)) +
  geom_col(
    width = 0.6,
    fill = "lightblue", color = 1) + 
  geom_text(
    aes(y = prob , label = letra), vjust = -0.6) +
  scale_y_continuous(
    name = "Probabilidade de ocorrer condensação (%)",
    labels = function(y) paste0(y, "%") ) +
  labs(
    x = "Bolsão"
  ) +
  theme_classic(base_size = 12) +
  theme(axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(size = 12))

ggsave(
  "Condensação.png",
  width = 6 ,
  height = 6,
  units = "in")

remove(resp,trat,a,letra)

################################################################################
#condiçao do engaço

perda = read_excel("RESULTADOS.xlsx",
                sheet = "PERDA" )


ggplot( perda, aes( x = BOLSAO, fill = ENGACO) ) +
  geom_bar( 
    aes( y= after_stat(count)),
    width = 0.7
  ) +
  geom_text(
    aes(
      y = after_stat(count),
      label = after_stat(paste0(round(count / tapply(count, x, sum)[x] * 100, 0), "%"))
    ),
    stat = "count",
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    x = "Bolsão",
    fill = "Desidratação") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    name = "") +
  fill_palette(
    c("#48BC66","#ECE253","#EC5454")
  )+
  theme_classic(base_size = 12) +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(size = 12))

ggsave(
  "engaco.png",
  width = 6 ,
  height = 6,
  units = "in"
)

################################################################################
#PERDA
with( 
  perda,
  DIC(
    BOLSAO,
    `PERDA_%`*100,
    mcomp = "tukey",
    CV = FALSE,
    width.bar = 0.1,
    width.column = 0.6,
    ylab = "Perda após 30 dias ('%')",
    xlab = "Bolsão"
    )
)

resp = perda$`PERDA_%`*100
trat = perda$BOLSAO

b = aov(resp ~ trat)

letra = 
  cld(
  regrid(
    emmeans( b, "trat", level = 0.95) 
    ),
  Letters = letters, 
  reversed = TRUE, 
  adjusted = "tukey"
)


letra %>% 
  select(-c(3,4)) %>% 
  rename(
    media = 2,
    inferior = 3,  
    superior = 4,  
    grupo = 5
  ) %>% 
  mutate(
    teste = paste( round( media, 2 ), "%", grupo , sep = "" ) ) %>% 
  ggplot( aes(x=trat, y= media) ) +
  geom_col( 
    stat = "identity",
    color= "black",
    fill = "lightblue",
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin =  inferior ,
      ymax = superior
    ),
    stat = "identity",
    width = 0.1
  ) +
  geom_text(
    aes(y = superior , label = teste),
    vjust = -0.6
    ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    name = "Perda (%)",
    expand = expansion(mult = c(0, 0.2)) 
  ) +
  labs( x ="Bolsão") +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
    strip.text = element_text(size = 12)
  )

ggsave(
  "perda.png",
  width = 6 ,
  height = 6,
  units = "in"
)

remove(trat, resp,letra, b)

################################################################################
#TERMOGRAFO
termografo = read_excel("RESULTADOS.xlsx",
                   sheet = "TERMOGRAFO" )

termografo$Horario = as.POSIXct(termografo$Horario)

plot1 <-
  termografo %>% 
    filter( Bolsao %in% c("MICRO 0,3%") ) %>% 
  ggplot( aes( x = Horario) ) +
  geom_line(
    aes(y = `UR (%)`*0.5, color = "UR (%)")
  ) +
  facet_wrap(~Bolsao, nrow = 1 ) +
  geom_line( aes(y = `T (°C)`, color = "Temperatura (ºC)") ) +
  scale_y_continuous(
    limits = c(0,44),
    breaks = seq(0, 44, by = 5),
    minor_breaks = seq(0, 44, by = 1),
    labels = function(x) paste0(x, "º"),
    sec.axis =  sec_axis(~./0.5,
                         labels = function(x) paste0(x, "%") )
    ) +
  scale_x_datetime(
    "Período do teste",
    date_labels = "%Hh\n%d/%m",
    date_breaks = "4 hour",
    minor_breaks = "1 hour"
  ) +
  labs(x= "Período do teste",
       y = "",
       color = "") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12))


plot2 <-
  termografo %>% 
  filter( Bolsao %in% c("MACRO 0,6%") ) %>% 
  ggplot( aes( x = Horario) ) +
  geom_line(
    aes(y = `UR (%)`*0.5, color = "UR (%)")
  ) +
  facet_wrap(~Bolsao, nrow = 1 ) +
  geom_line( aes(y = `T (°C)`, color = "Temperatura (ºC)") ) +
  scale_y_continuous(
    limits = c(0,44),
    breaks = seq(0, 44, by = 5),
    minor_breaks = seq(0, 44, by = 1),
    labels = function(x) paste0(x, "º"),
    sec.axis =  sec_axis(~./0.5,
                         labels = function(x) paste0(x, "%") )
  ) +
  scale_x_datetime(
    "",
    date_labels = "%Hh\n%d/%m",
    date_breaks = "4 hour",
    minor_breaks = "1 hour"
  ) +
  labs(x= "",
       y = "",
       color = "") +
  theme_bw(base_size = 12) +
  theme(legend.position = "top",
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12))

plot2/plot1


ggsave( "termografos.png",
        width = 10 ,
        height = 10,
        dpi = 300,
        units = "in"
)