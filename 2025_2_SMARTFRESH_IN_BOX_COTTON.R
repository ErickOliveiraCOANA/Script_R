library(AgroR)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ExpDes.pt)


library(tidyverse)
library(lubridate)

setwd("R:/Controle de Qualidade/TESTES/2025/SMARTFRESH IN THE BOX")


df <- read_excel("DADOS TABELADOS.xlsx", sheet = "ESTATISTICA" , range = "A1:K61")




mod = aov( perda_perc_45~termografo, data = df)

anova(mod)

with( df,
  DIC( trat = tratamento,
       response = perda_perc_45*100,
       ylab = "Perda após 45 dias (%)",
       width.column = 0.4,
       width.bar = 0.1,
       CV = FALSE,
       dec = 3
        ) )

ggsave( "perda.svg")



with( df,
      DIC( trat = termografo,
           response = perda_perc_45*100,
           ylab = "Perda após 45 dias (%)",
           width.column = 0.4,
           width.bar = 0.1,
           CV = FALSE,
           dec = 1
      ) )

ggsave( "perda_2.svg")

dbc( df$tratamento,
     df$perda_perc_45
     )


tabela = df %>% 
  group_by(tratamento) %>% 
  summarise(perda =mean(perda_perc_45*100),
            ds = sd(perda_perc_45*100))








ggplot(data= df, aes(x= tratamento, y= perda_perc_45*100))+
  geom_bar( stat = "summary", 
            fun = "mean",
            width = 0.6,
            fill = "lightblue",
            color = "black") +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.3 ) +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 1)), vjust = -10) +
  ylim(0,3) +
  labs(y="Perda após 45 dias (%)"  ) +
theme_classic()




geom_text(
  stat = "summary",
  fun.data = function(x) {
    m <- mean(x)
    sd <- sd(x)
    data.frame(y = m + sd + 5, label = round(m, 1))
  },
  aes(label = ..label..),
  vjust = 0
)


#######################################################################################

degrana <- read_excel("DADOS TABELADOS.xlsx", sheet = "ESTATISTICA" , range = "S1:V7")

with( degrana,
      DIC( trat = tratamento,
           response = degrana$degrana_perc,
           test = "noparametric",
           p.adj = "bonferroni",
           mcompNP = "LSD",
           ylab = "Degrana após 45 dias (%)",
           width.column = 0.4,
           width.bar = 0.1,
           CV = FALSE,
           dec = 3
           ))

ggsave( "degrana.svg")


ggplot(data=degrana, aes(x=Amostras,y=degrana_perc, fill = tratamento))+
  geom_bar(stat = "identity")


hist(degrana$degrana_perc)





#########################################
library(tidyr)
library(dplyr)
library(scales)

df %>% 
  select( tratamento,
          termografo,
          cumbuca,
          "30"=perda_perc_30,
          "45"=perda_perc_45) %>% 
  pivot_longer(cols = c("30", "45"),
               names_to = "dias",
               values_to = "perda_perc") %>% 
ggplot( aes(x= as.numeric(dias), y= perda_perc*100) ) +
  geom_line(stat = "summary", fun = "mean")+
  geom_point(stat = "summary", fun = "mean")+
  scale_y_continuous(
    name = "Perda (%)",
    labels = label_percent(scale = 1) ) +
  scale_x_continuous(name = "Dias")+
  theme_bw() +
  theme(
    axis.text = element_text(color = "black", size = 12 ),
    axis.title = element_text(color = "black", size = 12 ) )+
  facet_wrap(~tratamento,ncol=1)

ggsave("perda_dias.png",
       width = 8,
       height = 8,
       units = "in"
       )










#######################################################################################
#termografo
termografo <- read_excel("DADOS TABELADOS.xlsx", 
                         sheet = "termografos")


termografo$data = as.POSIXct(termografo$data)


glimpse(termografo)



termografo %>% filter(Bolsão %in% c("MACRO 0,6%","MICRO 0,6%") ) #filtrar


ggplot( data = termografo,
        aes( x = data, y = `Temperatura (°C)`, color = "Temperatura (°C)") ) +
  geom_line() +
  geom_line(aes(y = `UR (%)`*0.5, color = "UR (%)") ) +
  facet_wrap(~caixa, ncol = 1 ) +
  scale_y_continuous(
    limits = c(0,50),
    breaks = seq(0, 50, by = 5),
    labels = function(x) paste0(x, "º"),
    sec.axis = sec_axis(~./0.5,
                        labels = function(x) paste0(x, "%")) ) +
  labs(x= "Período do teste", y = "",
       color = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(size = 10) )

ggsave( "caixa_temp_umid.png",
        width = 10 ,
        height = 18,
        dpi = 300,
        units = "in"
)






termografo %>%
  mutate(data = floor_date(data, unit = "hour")) %>% # Arredonda para a hora cheia
  group_by(tratamento, data) %>%
  summarise(
    `Temperatura (°C)` = mean(`Temperatura (°C)`, na.rm = TRUE),
    `UR (%)` = mean(`UR (%)`, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ggplot( aes( x = data, y = `Temperatura (°C)`, color = "Temperatura (°C)") ) +
  geom_line() +
  geom_line(aes(y = `UR (%)`*0.5, color = "UR (%)") ) +
  facet_wrap(~tratamento, ncol=1 ) +
  scale_y_continuous(
    limits = c(0,50),
    breaks = seq(0, 50, by = 5),
    labels = function(x) paste0(x, "º"),
    sec.axis = sec_axis(~./0.5,
                        labels = function(x) paste0(x, "%")) ) +
  labs(x= "Período do teste", y = "",
       color = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(size = 10))

ggsave( "temperatura_umidade.png",
        width = 10 ,
        height = 15,
        dpi = 300,
        units = "in"
)




##############################################################
#engaço

engaco = read_excel("DADOS TABELADOS.xlsx", 
                    sheet = "ENGAÇO", range = "A14:E20")


names(engaco)


engaco %>% 
  pivot_longer(cols = c("leve","moderado","severo"),
               names_to = "Condição do Engaço",
               values_to = "perc") %>% 
ggplot( aes( x = caixa, y = perc*100, fill = `Condição do Engaço` ) ) +
  geom_bar( stat = "identity" ) +
  geom_text(  aes( label = paste0(round(perc*100,2),"%") ), position = position_stack(vjust = 0.5)  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    name = "") +
  theme_classic() +
  theme(legend.position = "top",
        legend.text = element_text(size = 11),
        axis.title = element_text(color = "black", size = 11),
        axis.text = element_text(color = "black", size = 11),
        strip.text = element_text(size = 10)) +
  fill_palette(c("#48BC66","#ECE253","#EC5454"))

ggsave( "condicao_engaco_1.png",
        width = 8 ,
        height = 8,
        dpi = 300,
        units = "in" )




engaco2 = read_excel("DADOS TABELADOS.xlsx", 
                    sheet = "ENGAÇO", range = "I1:L3")

engaco2 %>% 
  pivot_longer(cols = c("leve","moderado","severo"),
               names_to = "Condição do Engaço",
               values_to = "perc") %>% 
  ggplot( aes( x = tratamento, y = perc*100, fill = `Condição do Engaço` ) ) +
  geom_bar( stat = "identity",
            width = 0.4 ) +
  geom_text(  aes( label = paste0(round(perc*100,2),"%") ), position = position_stack(vjust = 0.5)  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    name = "") +
  theme_classic() +
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        axis.title = element_text(color = "black", size = 10),
        axis.text = element_text(color = "black", size = 10),
        strip.text = element_text(size = 9)) +
  fill_palette(c("#48BC66","#ECE253","#EC5454"))

ggsave( "condicao_engaco_2.png",
        width = 5 ,
        height = 5,
        dpi = 300,
        units = "in" )

