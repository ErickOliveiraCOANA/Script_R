library(readxl)
library(AgroR)
library(ggplot2)

setwd("R:/Controle de Qualidade/TESTES/2026/Cotton Candy Sasaki Abril")

dataset <- read_excel("R:/Controle de Qualidade/TESTES/2026/Cotton Candy Sasaki Abril/AVALIACAO.xlsx")

ggplot(dataset, aes(x = TRATAMENTO, y = `DEFEITOS %` * 100)) +
  geom_col(fill = "lightblue", colour = "black") +
  geom_text(
          aes(
            label = scales::number( 
              `DEFEITOS %` * 100,
              accuracy = 0.01,
              decimal.mark = ",",
              big.mark = ".",
              suffix = "%"
            )
          ), colour = "black", vjust = -0.3, size = 3.5 ) +
  labs( x = "Tratamentos", y = "Percentual de Defeitos (%)" ) +
  scale_y_continuous( 
    limits = c(0, 18),
    breaks = seq(0, 18, by = 2.5),
    labels = scales::label_number( decimal.mark = ",", big.mark = ".", suffix = "%" ) ) +
  theme_classic() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1,color = "black", size = 11),
         axis.text.y = element_text(color = "black", size = 11),
         axis.title = element_text(color = "black", size = 11))


ggsave( "grafico.png",
        width = 8 ,
        height = 7,
        dpi = 300,
        units = "in"
)



"
Material
ABS : Absorpede
PPS : Papel Seda

Localização
Acima: acima do generador
Meio: entre o generador e cumbucas
Abaixo: abaixo da cumbucas
"