library(ggplot2)
library(tidyverse)
library(AgroR)


setwd("R:/Controle de Qualidade/TESTES/2026/TESTE DE CAIXAS 8KG")

dataset$datahora = as.POSIXct(dataset$datahora,
                              format = "%d/%m/%Y %H:%M:%OS",
                              tz = "UTC")
dataset = 
  dataset %>% 
  mutate(
    Caixa = ifelse( Caixa == "RIKARU", "Rikaru", "Teste" )
  )

dataset = dataset %>%
  mutate(
    Name = case_when(
      Name == "1_TESTE.xlsx"  ~ "1_Teste",
      Name == "2_TESTE.xlsx"  ~ "2_Teste",
      Name == "3_RIKARU.xlsx" ~ "3_Rikaru",
      Name == "4_RIKARU.xlsx" ~ "4_Rikaru",
      TRUE ~ Name
    )
  )

################################################################################
#Temperatura_Termo_Amb
dataset  %>% 
  filter( datahora >= as.POSIXct("2026-09-25 23:00:00") &
            datahora <  as.POSIXct("2026-09-26 22:00:00") ) %>% 
  ggplot( aes(x = datahora ) ) +
  geom_line( stat = "summary", fun  = "mean", aes(y = `Temperatura (°C)`, colour = Caixa), linewidth = 0.8) +
  geom_line( stat = "summary", fun  = "mean", aes(y = PO, colour = "Ponto de Orvalho"), linewidth = 0.8) +
  geom_vline( xintercept = as.POSIXct("2026-09-26 14:40:00"), linetype = 2 ) +
  annotate(
    geom = "text",
    label = "Túnel 1",
    x = as.POSIXct("2026-09-26 5:30:00"),
    y = 30 ) +
  annotate(
    geom = "text",
    label = "Estoque 2",
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
      "Rikaru" = "green",
      "Teste" = "red",
      "Ponto de Orvalho" = "purple"
    ),
    breaks = c("Teste", "Rikaru", "Ponto de Orvalho"),
    name = NULL
  ) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(size = 12, hjust = 0.5)
  )

ggsave(
  "Temperatura_Termo_Amb.png",
  width = 12,
  height = 8,
  dpi = 300,
  units = "in"
)

################################################################################
#Temperatura_Termo_Polpa
dataset  %>% 
  filter( datahora >= as.POSIXct("2026-09-25 23:00:00") &
            datahora <  as.POSIXct("2026-09-26 22:00:00") ) %>% 
  ggplot( aes(x = datahora ) ) +
  geom_line( stat = "summary", fun  = "mean", aes(y = `Temperatura (°C)`, colour = Name), linewidth = 0.8) +
  geom_line( stat = "summary", fun  = "mean", aes(y = PO, colour = "Ponto de Orvalho"), linewidth = 0.8) +
  geom_vline( xintercept = as.POSIXct("2026-09-26 14:40:00"), linetype = 2 ) +
  annotate(
    geom = "text",
    label = "Túnel 1",
    x = as.POSIXct("2026-09-26 5:30:00"),
    y = 30 ) +
  annotate(
    geom = "text",
    label = "Estoque 2",
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
      "1_Teste" = "red",
      "2_Teste" = "salmon",
      "3_Rikaru" = "green",
      "4_Rikaru" = "olivedrab2",
      "Ponto de Orvalho" = "purple"
    ),
    name = NULL
  ) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(size = 12, hjust = 0.5)
  )

ggsave(
  "Temperatura_Termo_Polpa.png",
  width = 12,
  height = 8,
  dpi = 300,
  units = "in"
)

################################################################################
#Variação de Temperatura (Caixa Teste-Rikaru)
ggplot(dataset,
       aes( y = `Delta t polpa`, x = 0 )) +
  geom_boxplot(
    width = 0.6,
    fill = "lightblue"
  ) +
  labs( y = "Variação de Temperatura \n (Caixa Teste-Rikaru)", x = NULL) +
  stat_summary(
    fun = mean,
    fun.min = mean,
    fun.max = mean,
    geom = "errorbar",
    aes(color = "Média"),
    width = 0.6,
    linetype = "dashed",
    linewidth = 1,
    na.rm = TRUE
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Média" = "red")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "top"
  )

ggsave(
  "variacao_delta_no_tunel_T.png",
  width = 3,
  height = 8,
  dpi = 300,
  units = "in"
)