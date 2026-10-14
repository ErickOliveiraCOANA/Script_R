library(readxl)
library(tidyverse)
library(scales)


termografo <- read_excel("R:/Controle de Qualidade/TESTES/2025/TEMPERATURA E UMIDADE - TERMÓGRAFOS/ACOMPANHAMENTO DE TEMPERATURA - DADOS.xlsx", 
                         sheet = "Planilha2", col_types = c("text", "text", "numeric", "numeric", "text", "date") )


termografo$Horário = as.POSIXct(termografo$Horário)


glimpse(termografo)


ggplot( data = termografo,
        aes( x = `data/hora`, y = `Temperatura (°C)`, color = "Temperatura (°C)") ) +
  geom_line() +
  scale_x_datetime(
    date_breaks = "12 hour",
    date_labels = "%H \n%b %d"
  ) +
  geom_line(aes(y = `UR (%)`*0.5, color = "UR (%)") ) +
  facet_wrap(~Caixa, ncol = 1 ) +
  scale_y_continuous(
    limits = c(0,50),
    breaks = seq(0, 50, by = 5),
    labels = function(x) paste0(x, "º"),
    sec.axis = sec_axis(~./0.5,
                        labels = function(x) paste0(x, "%")) ) +
  labs(x= "Data/hora", y = "",
       color = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 8),
        strip.text = element_text(size = 10) )

ggsave( "temp_e_umid.png",
        width = 15 ,
        height = 15,
        dpi = 300,
        units = "in"
)






termografo %>%
  mutate(data = floor_date(`data/hora`, unit = "hour")) %>% # Arredonda para a hora cheia
  group_by(Caixa, `data/hora`) %>%
  summarise(
    `Temperatura (°C)` = mean(`Temperatura (°C)`, na.rm = TRUE),
    `UR (%)` = mean(`UR (%)`, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ggplot( aes( x = `data/hora`, y = `Temperatura (°C)`, color = "Temperatura (°C)") ) +
  geom_line() +
  geom_line(aes(y = `UR (%)`*0.5, color = "UR (%)") ) +
  facet_wrap(~Caixa, ncol=1 ) +
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
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 0.5) )

ggsave( "temperatura_umidade.png",
        width = 10 ,
        height = 15,
        dpi = 300,
        units = "in"
)





#########################################
#aferição QC

termografo2 <- read_excel("R:/Controle de Qualidade/TESTES/2025/TEMPERATURA E UMIDADE - TERMÓGRAFOS/ACOMPANHAMENTO DE TEMPERATURA - DADOS.xlsx", 
                       sheet = "Planilha3"
                       )


termografo2$DATA = as.POSIXct(termografo2$DATA)

termografo2$`Início da Hora` <- as.POSIXct(termografo2$`Início da Hora`, tryFormats = "%d/%m/%Y %H:%M:%S")


ggplot( data = termografo2,
        aes( x = `Início da Hora`) ) +
  geom_line( aes( y = `Aferição QC`, color = "Aferição QC")  ) +
  geom_line( aes( y = Termógrafo, color = "Termógrafo") ) +
  facet_wrap(~CLASSIFICAÇÃO, ncol = 1 ) +
  labs(x = NULL, colour = NULL) +
  scale_y_continuous( name = "Temperatura (ºC)",
    labels = function(x) paste0(x, "º") ) +
  labs(x= "Periodo de teste", y = "",
       color = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(size = 10) )


ggsave( "QC+termografo.png",
        width = 12 ,
        height = 10,
        dpi = 300,
        units = "in" )


#########################################
#aferição QC

tunel <- read_excel("R:/Controle de Qualidade/TESTES/2025/TEMPERATURA E UMIDADE - TERMÓGRAFOS/ACOMPANHAMENTO DE TEMPERATURA - DADOS.xlsx", 
                          sheet = "TUNEL", range = "A2:I12" )


tunel$HORÁRIO <- format(tunel$HORÁRIO, "%H:%M")


ggplot( data = tunel,
        aes( x = `DATA/HORA` ) ) +
  geom_line( aes( y = `Aferição Túnel`, color = "Aferição Túnel")  ) +
  geom_line( aes( y = Termógrafo, color = "Termógrafo") ) +
  facet_wrap(~CLASSIFICAÇÃO, ncol = 1 ) +
  scale_y_continuous( name = "Temperatura (ºC)",
                      labels = function(x) paste0(x, "º") ) +
  labs(x= "Periodo de teste", y = "",
       color = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(size = 10) )


ggsave( "tunel+termografo.png",
        width = 12 ,
        height = 10,
        dpi = 300,
        units = "in" )
