
library(ggplot2)
library(tidyverse)
library(AgroR)

setwd("R:/Controle de Qualidade/TESTES/2026/Branqueamento na Candy Snaps")

dataset$datahora = as.POSIXct(dataset$datahora,
                              format = "%d/%m/%Y %H:%M:%OS",
                              tz = "UTC")

################################################################################
dataset  %>% 
  filter( datahora >= as.POSIXct("2026-09-16 10:00:00") &
            datahora <  as.POSIXct("2026-09-17 6:00:00") ) %>% 
  ggplot( aes(x = datahora ) ) +
  geom_line( aes(y = `Temperatura (°C)`, colour = "Caixa"), linewidth = 0.8) +
  geom_line( aes(y = temp_entrada, colour = "Ambiente"), linewidth = 0.8) +
  geom_line( aes(y = PO, colour = "P. Orvalho Amb"), linewidth = 0.8) +
  geom_line( aes(y = `PO.1`, colour = "P. Orvalho Cai"), linewidth = 0.8) +
  scale_x_datetime(
    date_breaks = "2 hour",
    date_labels = "%Hh \n%b %d"
  ) +
  labs(
    x = ""
  ) +
  scale_color_manual(
    values = c(
      "Caixa" = "red",
      "Ambiente" = "orange",
      "P. Orvalho Amb" = "#40D184",
      "P. Orvalho Cai" = "purple"
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
  geom_line( aes(y = UR, color = "Caixa"), linewidth = 0.8) +
  geom_line( aes(y = umidade, color = "Ambiente"), linewidth = 0.8) +
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

