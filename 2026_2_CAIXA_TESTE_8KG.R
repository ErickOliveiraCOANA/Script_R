
library(ggplot2)
library(tidyverse)
library(AgroR)


setwd("R:/Controle de Qualidade/TESTES/2026/TESTE DE CAIXAS 8KG")

dataset$datahora = as.POSIXct(dataset$datahora,
                              format = "%d/%m/%Y %H:%M:%OS",
                              tz = "UTC")

################################################################################
dataset  %>% 
  filter( datahora >= as.POSIXct("2026-09-25 23:00:00") &
            datahora <  as.POSIXct("2026-09-26 22:00:00") ) %>% 
  ggplot( aes(x = datahora ) ) +
  geom_line( stat = "summary", fun  = "mean", aes(y = `Temperatura (°C)`, colour = Caixa), linewidth = 0.8) +
  geom_line( stat = "summary", fun  = "mean", aes(y = PO, colour = "P. Orvalho Caixa"), linewidth = 0.8) +
  geom_vline( xintercept = as.POSIXct("2026-09-26 14:40:00"), linetype = 2 ) +
  annotate(
    geom = "text",
    label = "Túnel 01",
    x = as.POSIXct("2026-09-26 5:30:00"),
    y = 30 ) +
  annotate(
    geom = "text",
    label = "Estoque",
    x = as.POSIXct("2026-09-26 18:30:00"),
    y = 30 ) +
  scale_x_datetime(
    date_breaks = "2 hour",
    date_labels = "%Hh \n%b %d"
  ) +
  labs(
    x = ""
  ) +
  scale_color_manual(
    values = c(
      "RIKARU" = "red",
      "TESTE" = "orange",
      "P. Orvalho Caixa" = "purple"
    ),
    name = NULL
  ) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(size = 12, hjust = 0.5)
  )

ggsave(
  "temp.png",
  width = 10 ,
  height = 7,
  units = "in")

################################################################################
dataset  %>% 
  filter( datahora >= as.POSIXct("2026-09-16 10:00:00") &
            datahora <  as.POSIXct("2026-09-17 6:00:00") ) %>% 
  ggplot( aes(x = datahora ) ) +
  geom_line( stat = "summary", fun  = "mean", aes(y = UR, color = "Caixa"), linewidth = 0.8) +
  geom_line( stat = "summary", fun  = "mean", aes(y = umidade, color = "Ambiente"), linewidth = 0.8) +
  scale_x_datetime(
    date_breaks = "2 hour",
    date_labels = "%Hh \n%b %d"
  ) +
  scale_color_manual(
    values = c(
      "Caixa" = "lightblue",
      "Ambiente" = "blue"),
    name = NULL
  ) +
  labs(
    x = ""
  ) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(size = 12, hjust = 0.5)
  )


ggsave(
  "umi.png",
  width = 10 ,
  height = 7,
  units = "in")

################################################################################
dataset  %>% 
  filter( datahora >= as.POSIXct("2026-09-25 23:00:00") &
            datahora <  as.POSIXct("2026-09-26 22:00:00") ) %>% 
  ggplot( aes(x = datahora ) ) +
  geom_line( stat = "summary", fun  = "mean", aes(y = `Temperatura (°C)`, colour = Name), linewidth = 0.8) +
  geom_line( stat = "summary", fun  = "mean", aes(y = PO, colour = "P. Orvalho Caixa"), linewidth = 0.8) +
  geom_vline( xintercept = as.POSIXct("2026-09-26 14:40:00"), linetype = 2 ) +
  annotate(
    geom = "text",
    label = "Túnel 01",
    x = as.POSIXct("2026-09-26 5:30:00"),
    y = 30 ) +
  annotate(
    geom = "text",
    label = "Estoque",
    x = as.POSIXct("2026-09-26 18:30:00"),
    y = 30 ) +
  scale_x_datetime(
    date_breaks = "2 hour",
    date_labels = "%Hh \n%b %d"
  ) +
  labs(
    x = ""
  ) +
  scale_color_manual(
    values = c(
      "1_TESTE.xlsx" = "blue",
      "2_TESTE.xlsx" = "green",
      "3_RIKARU.xlsx" = "red",
      "4_RIKARU.xlsx" = "orange",
      "P. Orvalho Caixa" = "purple"
    ),
    name = NULL
  ) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(size = 12, hjust = 0.5)
  )



###############################################################################
#delta T do sensor de ambiente dentro da caixa
ggplot(dataset,
       aes( y = `Delta t polpa`, x = 0 )) +
  geom_boxplot(
    width = 0.6,
    fill = "lightblue"
  ) +
  geom_point(
    stat = "summary",
    fun = "mean",
    shape = 17,
    size = 4,
    fill = 17
  ) +
  theme_bw()