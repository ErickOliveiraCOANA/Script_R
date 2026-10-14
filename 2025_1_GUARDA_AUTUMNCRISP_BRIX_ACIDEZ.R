library(ggplot2)
library(ggpubr)
library(ExpDes.pt)
library(dplyr)
library(tidyr)
library(AgroR)
library(readxl)
library(bestNormalize)

BS <- read_excel("C:/Users/erick/Downloads/Compressed/TESTE DE GUARDA AUTUMNCRISP - FAN - 15-10-2024/ESTATISTICA.xlsx", 
                          sheet = "BRIX_ACIDEZ")

boxplot( BS$BRIX ~BS$CAIXA, col = BS$CLASSIFICACAO_BRIX
)

with( BS,
DIC( CAIXA , bn$x.t, errorbar = FALSE, mcomp = "sk"
     )
)

edit(BS)

BS <-bn$x.t

bn<- bestNormalize(BS$BRIX)

plot(bn, leg_loc = "bottomright")
boxplot( bn$x.t ~BS$CAIXA
  )

with(BS, transf(BRIX, f1=CAIXA))

hist(bn$x.t)


###############################################################
PB <- read_excel("C:/Users/erick/Downloads/Compressed/TESTE DE GUARDA AUTUMNCRISP - FAN - 15-10-2024/ESTATISTICA.xlsx", 
                 sheet = "PROBLEMAS_6_CAIXAS")

PB <- pivot_longer(data = PB, cols = c(2:10), names_to = "Defeitos", values_to = "Valor" )





po <-  read_excel("C:/Users/erick/Downloads/Compressed/TESTE DE GUARDA AUTUMNCRISP - FAN - 15-10-2024/ESTATISTICA.xlsx", 
                  sheet = "POSICAO")


po[is.na(po)] <- 0

po <- po %>% select(-"Queima de sol")  

edit(po)
head(po)

po1 <- po %>%  group_by(POSICAO) %>% # Agrupar pela coluna POSICAO
  summarize(
    Cicatrizes = sum(Cicatrizes, na.rm = TRUE),
    Mecanico = sum(Mecanico, na.rm = TRUE),
    Murcha = sum(Murcha, na.rm = TRUE),
    Rachada = sum(Rachada, na.rm = TRUE),
    Podre = sum(Podre, na.rm = TRUE),
    Congelamento = sum(Congelamento, na.rm = TRUE),
    SO2 = sum(SO2, na.rm = TRUE),
    Fungo = sum(Fungo, na.rm = TRUE)
  )

po1 <- pivot_longer(data = po1, cols = c(2:9), names_to = "Defeitos", values_to = "Valor" )


with( po,
      DIC( POSICAO , bn$x.t, errorbar = FALSE, mcomp = "sk"
      )
)
                    