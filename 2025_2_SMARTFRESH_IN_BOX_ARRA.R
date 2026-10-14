library(AgroR)
library(readxl)
require(ggplot2)
library(ggpubr)

setwd("Z:/Controle de Qualidade/TESTES/2025/SMARTFRESH IN BOX/ARRAFIFTEEN")

##########################################################
#PERDA
perdas = read_xlsx( path = "DADOS TABELADOS.xlsx",
                  sheet = "ESTATÍSTICA",
                  range = "A1:F41"
  )


#modelo de regressão linear
mod = aov(`PERDA_%`~TRATAMENTO, data = perdas)

#teste de normalidade
shapiro.test(mod$residuals)

#teste de homogeneidade
bartlett.test(`PERDA_%`~TRATAMENTO, data = perdas)

#teste de Durbin-Watson test
library(lmtest)

dwtest(`PERDA_%`~TRATAMENTO, data = perdas)

anova(mod)

with(data = perdas,
     DIC( trat = TRATAMENTO,
          response = -1*`PERDA_%`,
          ylab = "Perda (%)",
          width.column = 0.6,
          transf = 0,
          width.bar = 0.1,
          CV = FALSE,
          dec = 3
  )
)

ggplot( data = perdas,
        aes( x = TRATAMENTO, y = -1*`PERDA_%` ) ) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6,
            fill = "lightblue",
            color = "black"
            ) +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.1,
                 color = "black"
                 ) +
  scale_y_continuous(
    name = "Perda (%)",
    labels = function(x) paste0(x, "%") ) + labs( x= "" ) +
  theme_classic() +
  theme(
        legend.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(size = 10) )


ggsave( "perda_ARRA2.png")



###############################################################################
#DEGRANA
dados2 = read_xlsx( path = "DADOS TABELADOS.xlsx",
                    sheet = "ESTATÍSTICA",
                    range = "K1:O5"
                    )

#modelo de regressão linear
mod = aov(DEGRANA~TRATAMENTO, data = dados2)

#teste de normalidade
shapiro.test(mod$residuals)

#teste de homogeneidade
bartlett.test(DEGRANA~TRATAMENTO, data = dados2)


with( data = dados2,
      DIC( TRATAMENTO,
          round(PERC_DEGRANA*100,2),
           ylab = "Degrana (%)",
           width.column = 0.6,
           width.bar = 0.1,
           CV = FALSE,
           dec = 3
      )
)

ggsave( "DEGRANA_ARRA23.svg")


ggplot( data = dados2,
        aes( x = TRATAMENTO, y = round(PERC_DEGRANA*100,2) ) ) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6,
            fill = "lightblue",
            color = "black"
  ) +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.1,
                 color = "black"
  ) +
  scale_y_continuous(
    name = "Degrana (%)",
    labels = function(x) paste0(x, "%") ) + labs( x= "" ) +
  theme_classic() +
  theme(
    legend.text = element_text(size = 12),
    axis.title = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
    strip.text = element_text(size = 10) )

ggsave( "DEGRANA_ARRA2.svg")